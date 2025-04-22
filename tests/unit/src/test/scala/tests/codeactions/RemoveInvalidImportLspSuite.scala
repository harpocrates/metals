package tests.codeactions

import scala.meta.internal.metals.codeactions.RemoveInvalidImport

class RemoveInvalidImportLspSuite
    extends BaseCodeActionLspSuite("removeInvalidSymbol") {

  check(
    "entire-import-1",
    """|package a
       |
       |<<import scala.collection.Seq
       |import scala.collection.DoesntExist>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("DoesntExist")}
        |""".stripMargin,
    """|package a
       |
       |import scala.collection.Seq
       |
       |object A
       |""".stripMargin,
  )

  check(
    "entire-import-2",
    """|package a
       |
       |<<import scala.collection.Seq
       |import scala.collection.doesntExist.List>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("doesntExist")}
        |""".stripMargin,
    """|package a
       |
       |import scala.collection.Seq
       |
       |object A
       |""".stripMargin,
  )

  check(
    "entire-import-3",
    """|package a
       |
       |<<import scala.collection.Seq
       |import scala.collection.doesntExist.{List, LazyList}>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("doesntExist")}
        |""".stripMargin,
    """|package a
       |
       |import scala.collection.Seq
       |
       |object A
       |""".stripMargin,
  )

  check(
    "in-importer-1",
    """|package a
       |
       |<<import scala.doesntExist.Baz, scala.collection.Seq>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("doesntExist")}
        |""".stripMargin,
    """|package a
       |
       |import scala.collection.Seq
       |
       |object A
       |""".stripMargin,
  )

  check(
    "in-importer-2",
    """|package a
       |
       |<<import scala.collection.Seq, scala.doesntExist.Baz>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("doesntExist")}
        |""".stripMargin,
    """|package a
       |
       |import scala.collection.Seq
       |
       |object A
       |""".stripMargin,
  )

  check(
    "in-importer-3",
    """|package a
       |
       |<<import scala.collection.DoesntExist, scala.doesntExist.Baz>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("DoesntExist")}
        |${RemoveInvalidImport.title("doesntExist")}
        |""".stripMargin,
    """|package a
       |
       |
       |object A
       |""".stripMargin,
  )

  check(
    "in-selector-1",
    """|package a
       |
       |<<import scala.collection.{DoesntExist, Seq, IndexedSeq}>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("DoesntExist")}
        |""".stripMargin,
    """|package a
       |
       |import scala.collection.{Seq, IndexedSeq}
       |
       |object A
       |""".stripMargin,
  )

  check(
    "in-selector-2",
    """|package a
       |
       |<<import scala.collection.{Seq, DoesntExist, IndexedSeq}>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("DoesntExist")}
        |""".stripMargin,
    """|package a
       |
       |import scala.collection.{Seq, IndexedSeq}
       |
       |object A
       |""".stripMargin,
  )

  check(
    "in-selector-3",
    """|package a
       |
       |<<import scala.collection.{Seq, IndexedSeq, DoesntExist}>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("DoesntExist")}
        |""".stripMargin,
    """|package a
       |
       |import scala.collection.{Seq, IndexedSeq}
       |
       |object A
       |""".stripMargin,
  )

  check(
    "in-selector-4",
    """|package a
       |
       |<<import scala.collection.{DoesntExist1, DoesntExist2}>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("DoesntExist1")}
        |${RemoveInvalidImport.title("DoesntExist2")}
        |""".stripMargin,
    """|package a
       |
       |
       |object A
       |""".stripMargin,
  )

  check(
    "involved1",
    """|package a
       |<<
       |import scala.sys.doesntExit.other
       |import scala.collection.{DoesntExist1, AbstractSet, DoesntExist2}, scala.collection.DoesntExist3, scala.foo.DoesntExist4
       |import doesnt.exist
       |import _root_.scala.concurrent.Future
       |>>
       |
       |object A
       |""".stripMargin,
    s"""|${RemoveInvalidImport.allSymbolsTitle}
        |${RemoveInvalidImport.title("doesntExit")}
        |${RemoveInvalidImport.title("DoesntExist1")}
        |${RemoveInvalidImport.title("DoesntExist2")}
        |${RemoveInvalidImport.title("DoesntExist3")}
        |${RemoveInvalidImport.title("foo")}
        |${RemoveInvalidImport.title("doesnt")}
        |""".stripMargin,
    """|package a
       |
       |import scala.collection.{AbstractSet}
       |import _root_.scala.concurrent.Future
       |
       |object A
       |""".stripMargin,
    filterAction = action => action.getTitle().contains("invalid import"),
  )
}
