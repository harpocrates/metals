package scala.meta.internal.metals.codeactions

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.Import
import scala.meta.Importee
import scala.meta.Importer
import scala.meta.Tree
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ScalacDiagnostic
import scala.meta.internal.metals.codeactions.CodeAction
import scala.meta.internal.metals.codeactions.CodeActionBuilder
import scala.meta.internal.parsing.Trees
import scala.meta.pc.CancelToken

import org.eclipse.{lsp4j => l}

class RemoveInvalidImport(
    trees: Trees
) extends CodeAction {
  override def kind: String = l.CodeActionKind.QuickFix

  override def contribute(params: l.CodeActionParams, token: CancelToken)(
      implicit ec: ExecutionContext
  ): Future[Seq[l.CodeAction]] = {
    val path = params.getTextDocument().getUri().toAbsolutePath
    val range = params.getRange()

    sealed trait RemoveEdit
    object RemoveEdit {
      case object EntireTree extends RemoveEdit
      case class Parts(parts: Seq[l.Range]) extends RemoveEdit

      val Empty = Parts(Seq.empty)
    }

    def seqRemoveEdit(
        orderedElements: Iterator[(Tree, RemoveEdit)]
    ): RemoveEdit = {

      var seqIsFullyRemoved = true
      val removalRanges = Seq.newBuilder[l.Range]
      def addRemovalRanges(ranges: Seq[l.Range]) =
        for (range <- ranges; if !range.isOffset && !range.isNone) {
          removalRanges += range
        }

      // If the previous entry was removed, Left(full range of removal).
      // If the previous entry was present, Right(previous tree)
      var runningRemovalOrPrev: Either[l.Range, Tree] =
        orderedElements.next() match {
          case (firstElement, RemoveEdit.EntireTree) =>
            Left(firstElement.pos.toLsp)
          case (firstElement, RemoveEdit.Parts(parts)) =>
            seqIsFullyRemoved = false
            addRemovalRanges(parts)
            Right(firstElement)
        }

      for ((element, removeEdit) <- orderedElements) {
        (removeEdit, runningRemovalOrPrev) match {
          case (RemoveEdit.EntireTree, Left(runningRemoval)) =>
            val newRunningRemoval =
              new l.Range(runningRemoval.getStart(), element.pos.toLsp.getEnd())
            runningRemovalOrPrev = Left(newRunningRemoval)

          case (RemoveEdit.EntireTree, Right(prevTree)) =>
            val runningRemoval = new l.Range(
              prevTree.pos.toLsp.getEnd(),
              element.pos.toLsp.getEnd(),
            )
            runningRemovalOrPrev = Left(runningRemoval)

          case (RemoveEdit.Parts(parts), Left(runningRemoval)) =>
            addRemovalRanges(Seq(if (seqIsFullyRemoved) {
              new l.Range(runningRemoval.getStart, element.pos.toLsp.getStart())
            } else {
              runningRemoval
            }))
            seqIsFullyRemoved = false
            addRemovalRanges(parts)
            runningRemovalOrPrev = Right(element)

          case (RemoveEdit.Parts(parts), Right(_)) =>
            seqIsFullyRemoved = false
            addRemovalRanges(parts)
            runningRemovalOrPrev = Right(element)
        }
      }

      if (seqIsFullyRemoved) {
        RemoveEdit.EntireTree
      } else {
        runningRemovalOrPrev match {
          case Left(runningRemoval) => addRemovalRanges(Seq(runningRemoval))
          case Right(_) => ()
        }
        RemoveEdit.Parts(removalRanges.result())
      }
    }

    // Possibly remove invalid importee
    def removeInvalidFromImportee(
        importee: Importee,
        diags: Seq[(l.Range, String)],
    ): Option[(String, RemoveEdit)] = {
      val nameOpt = importee match {
        case Importee.Name(name) => Some(name.value)
        case Importee.Rename(name, _) => Some(name.value)
        case _ => None
      }
      lazy val importeePos = importee.pos.toLsp
      nameOpt
        .flatMap(name =>
          diags.find { case (r, n) => name == n && importeePos == r }
        )
        .map { case (_, invalidName) =>
          RemoveInvalidImport.title(invalidName) -> RemoveEdit.EntireTree
        }
    }

    // Remove invalid elements from importer
    // Return (all possible individual removals, result of removing everything)
    //
    //    IN:  {foo => bar, baz, box}
    //    OUT: [Remove("foo => bar, "), Remove(", box")], Remove("foo => bar,", ", box")
    def removeInvalidFromImporter(
        importer: Importer,
        diags: List[(l.Range, String)],
    ): (Seq[(String, RemoveEdit)], RemoveEdit) = {

      diags.headOption.find { case (r, _) =>
        importer.ref.pos.encloses(r)
      } match {
        case Some((_, invalidName)) =>
          (Seq(
            RemoveInvalidImport.title(invalidName) -> RemoveEdit.EntireTree
          ) -> RemoveEdit.EntireTree)
        case None =>
          val importees = importer.importees
          val importeeEdits = groupDiagnosticsByTree(diags, importees)
            .map { case (importee, diags) =>
              removeInvalidFromImportee(importee, diags)
            }

          val caseByCase = importeeEdits.zipWithIndex
            .collect { case ((Some(removal), idx)) => removal -> idx }
            .map { case ((title, removal), index) =>
              title -> seqRemoveEdit(importees.iterator.zipWithIndex.map {
                case (tree, treeIndex) =>
                  tree -> (if (treeIndex == index) removal
                           else RemoveEdit.Empty)
              })
            }
          val aggregate = seqRemoveEdit(
            importees.zip(importeeEdits).iterator.map {
              case (importee, editOpt) =>
                importee -> editOpt.fold[RemoveEdit](RemoveEdit.Empty)(_._2)
            }
          )
          caseByCase -> aggregate
      }
    }

    def removeInvalidFromImport(
        imprt: Import,
        diags: List[(l.Range, String)],
    ): (Seq[(String, RemoveEdit)], RemoveEdit) = {

      val importers = imprt.importers
      val (caseByCaseImporterEdits, allEdits) =
        groupDiagnosticsByTree(diags, importers).map { case (importer, diags) =>
          removeInvalidFromImporter(importer, diags)
        }.unzip

      val caseByCase = caseByCaseImporterEdits.zipWithIndex
        .flatMap { case (removals, index) =>
          removals.map { case (title, removal) =>
            title -> seqRemoveEdit(importers.iterator.zipWithIndex.map {
              case (tree, treeIndex) =>
                tree -> (if (treeIndex == index) removal else RemoveEdit.Empty)
            })
          }
        }
      val aggregate = seqRemoveEdit(
        importers.zip(allEdits).iterator
      )

      caseByCase -> aggregate
    }

    def importRemoveEditToTextEdit(
        imprt: Import,
        edit: RemoveEdit,
    ): Seq[l.Range] = {
      edit match {
        case RemoveEdit.EntireTree =>
          val importPos = imprt.pos
          val range = importPos.toLsp

          // If there is a newline right after the import, include that in the range
          if (importPos.input.chars.lift(importPos.end).contains('\n')) {
            val end = range.getEnd()
            range.setEnd(new l.Position(end.getLine() + 1, 0))
          }
          Seq(range)
        case RemoveEdit.Parts(parts) =>
          parts
      }
    }

    val possibleActionPoints = params
      .getContext()
      .getDiagnostics()
      .asScala
      .collect {
        case diag @ ScalacDiagnostic.SymbolNotFound(name)
            if range.overlapsWith(diag.getRange()) =>
          diag.getRange() -> name
        case diag @ ScalacDiagnostic.ObjectNotAMemberOfPackage(name)
            if range.overlapsWith(diag.getRange()) =>
          diag.getRange() -> name
      }
      .sortBy { case (r, _) =>
        r.getStart().getLine() -> r.getStart().getCharacter()
      }
    if (possibleActionPoints.isEmpty) {
      Future.successful(Nil)
    } else {}
    val codeActions = trees.get(path) match {
      case _ if possibleActionPoints.isEmpty => Nil
      case None => Nil
      case Some(rootTree) =>
        val importsAndDiags = groupDiagnosticsByTree(
          possibleActionPoints.toList,
          findImportsOverlappingWithRange(rootTree, range),
        )
        val (caseByCaseRemovals, fullRemovals) = importsAndDiags.map {
          case (imprt, diags) =>
            val (caseByCase, aggregate) = removeInvalidFromImport(imprt, diags)
            caseByCase.map { case (title, edits) =>
              title -> importRemoveEditToTextEdit(imprt, edits)
            } -> importRemoveEditToTextEdit(imprt, aggregate)
        }.unzip

        val individualRemovals = caseByCaseRemovals.flatten.map {
          case (title, removalRanges) =>
            CodeActionBuilder.build(
              title = title,
              kind = this.kind,
              changes = List(path -> removalRanges.map(new l.TextEdit(_, ""))),
            )
        }
        val aggregateRemoval = CodeActionBuilder.build(
          title = RemoveInvalidImport.allSymbolsTitle,
          kind = this.kind,
          changes =
            List(path -> fullRemovals.flatten.map(new l.TextEdit(_, ""))),
        )
        individualRemovals.prepended(aggregateRemoval)
    }
    Future.successful(codeActions)
  }

  private def findImportsOverlappingWithRange(
      root: Tree,
      range: l.Range,
  ): List[Import] = {
    val imports = List.newBuilder[Import]
    var toVisit = List(root)

    @tailrec
    def loop(): Unit =
      toVisit match {
        case tree :: rest =>
          toVisit = rest
          if (range.overlapsWith(tree.pos.toLsp)) {
            tree match {
              case imprt: Import => imports += imprt
              case other => toVisit ++= other.children
            }
          }
          loop()
        case Nil => ()
      }

    loop()
    imports.result()
  }

  // Pre-condition: diags and trees are sorted by starting position
  @tailrec
  private def groupDiagnosticsByTree[T <: Tree](
      diags: List[(l.Range, String)],
      subtrees: List[T],
      acc: List[(T, List[(l.Range, String)])] = Nil,
  ): List[(T, List[(l.Range, String)])] =
    subtrees match {
      case Nil => acc.reverse
      case tree :: remainingSubtrees =>
        val importRange = tree.pos.toLsp
        val (thisImport, remainingDiags) = diags.span { case (diagRange, _) =>
          importRange.encloses(diagRange.getStart())
        }
        val diagsForImport = thisImport.filter { case (diagRange, _) =>
          importRange.encloses(diagRange)
        }
        groupDiagnosticsByTree(
          remainingDiags,
          remainingSubtrees,
          (tree, diagsForImport) :: acc,
        )
    }
}

object RemoveInvalidImport {

  def title(name: String): String =
    s"Remove invalid import of '$name'"

  def allSymbolsTitle: String =
    s"Remove all invalid imports"
}
