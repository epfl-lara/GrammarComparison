package grammar.exercises
import grammar._
import scala.collection.mutable.ArrayBuffer
import CFGrammar._
import EBNFGrammar._
import java.io.File

object ExerciseType {
  //var uniqueId = 0
  object ExType {
    val registeredTypes = ArrayBuffer[ExType]()
    def unapply(s: String): Option[ExType] = registeredTypes.find(_.key == s)
  }

  //set of all possible exercises  
  sealed abstract class ExType {
    /*val id = {
      uniqueId += 1
      uniqueId
    }*/
    def key: String
    ExType.registeredTypes += this
  }
  val GrammarEx = new ExType {
    override def toString = "Grammar for a language"
    def key = "grammar"
  }
  val CNFEx = new ExType {
    override def toString = "Conversion to CNF"
    def key = "cnf"
  }
  val GNFEx = new ExType {
    override def toString = "Conversion to GNF"
    def key = "gnf"
  }
  val DerivationEx = new ExType {
    override def toString = "Derivation for a word"
    def key = "derivation"
  }
  val CYKEx = new ExType {
    override def toString = "CYK parsing"
    def key = "cyk"
  }
  val ProgLangEx = new ExType {
    override def toString = "Programming Language Grammars"
    def key = "proglang"
  }
  val AllGrammarsEx = new ExType {
    override def toString = "All grammars"
    def key = "all_grammars"
  }

  //TODO: Add CYKEx as well
  //Note: keep appending exercises so that we don't change the ids of the existing ones 
  //(the system does not depend on this, only to ensure consistency of stats)  
  val allExercises = List(GrammarEx, CNFEx, GNFEx, DerivationEx, ProgLangEx, CYKEx)

  def getExType(key: String) = {
    allExercises.find(_.key == key)
  }

  def parseWord(wordStrs: List[String], refGrammar: BNFGrammar): Either[String, Word] = {
    val (words, errmsg) = (new SententialFormParser()).parseSententialForms(
      wordStrs.toList, refGrammar.cfGrammar)
    if (!errmsg.isEmpty())
      Left("Error in parsing word: " + errmsg)
    else {
      val sforms = words.filter(_.exists(_.isInstanceOf[Nonterminal]))
      if (!sforms.isEmpty) {
        Left("Error: Found words with non-terminals: " + wordsToString(sforms))
      } else {
        //Here, we still do not know if the word is parsable by the grammar or not.
        //TODO: for this we need to use CYK parser here.
        Right(words(0).map(_.asInstanceOf[Terminal]))
      }
    }
  }
}

case class GrammarEntry(
  id: Int,
  name: String, // Title
  desc: String,
  reference: BNFGrammar,
  word: Option[Word],
  initGrammar: Option[BNFGrammar],
  usecases: Array[String],
  referenceFile: Option[String],
  initialFile: Option[String],
  maxGrade: Int = 100) {

  import ExerciseType._
  private[exercises] var dirtyInitialFile = false
  private[exercises] var dirtyReferenceFile = false
  def setToExportInitGrammar() = { dirtyInitialFile = true; this }
  def setToExportReference() = { dirtyInitialFile = true; this }

  lazy val extypes =
    (if (usecases.contains("all"))
      ExerciseType.allExercises.toSet - ProgLangEx
    else if (usecases.contains("nogrammar"))
      (ExerciseType.allExercises.toSet -- Set(GrammarEx, ProgLangEx))
    else
      usecases.collect { case ExType(exType) => exType }.toSet) + AllGrammarsEx
  lazy val refGrammar = reference.cfGrammar
  lazy val cnfRef = refGrammar.cnfGrammar
  lazy val isLL1Entry = GrammarUtils.isLL1(refGrammar)
}

sealed abstract class GrammarDatabase { self =>
  def file: java.io.File
  def dir: String
  def grammarEntries: Seq[GrammarEntry]

  def entriesForExercise(extype: ExerciseType.ExType) = {
    grammarEntries.filter(_.extypes.contains(extype))
  }

  /** Adds or updates a grammar entry based on the ID */
  def updated(g: GrammarEntry): GrammarDatabase = new GrammarDatabase {
    def file = self.file
    def dir = self.dir
    def grammarEntries = {
      var found = false
      self.grammarEntries.map { case ge => if (ge.id == g.id) { found = true; g } else ge }.toList ++
        (if (found) Nil else List(g))
    }
  }
  def deleted(id: Int): GrammarDatabase = new GrammarDatabase {
    def file = self.file
    def dir = self.dir
    def grammarEntries = self.grammarEntries.filter(_.id != id)
  }
}

sealed abstract class ImportGrammarDatabase(val file: java.io.File, val dir: String) extends GrammarDatabase {
  import ExerciseType._
  val grammarEntries = {
    val xmlRoot = scala.xml.XML.loadFile(file)
    val exers = (xmlRoot \ "grammar").map(exerciseElem => {
      val id = (exerciseElem \ "@id").text.toInt
      val usecases = (exerciseElem \ "@usecases").text.split(",")
      val title = (exerciseElem \ "title" \ "i").toString
      val desc = (exerciseElem \ "description" \ "p").toString

      //read the reference grammar (either from a file or directly)
      val refElems = (exerciseElem \ "reference")
      val refFiles = (exerciseElem \ "referenceFile")
      val (refGrammar, errmsg) = if (!refElems.isEmpty) {
        val rules = (refElems(0) \ "rule").map(_.text.trim)
        (new GrammarParser()).parseGrammar(rules.toList)
      } else {
        //see if we have a reference file
        if (!refFiles.isEmpty) {
          val file = dir + "/" + refFiles(0).text
          val lines = scala.io.Source.fromFile(file).getLines.toList
          (new GrammarParser()).parseGrammar(lines)
        } else
          throw new IllegalArgumentException("Reference grammar not specified in problem " + id)
      }
      if (!refGrammar.isDefined)
        throw new IllegalArgumentException("Error in parsing reference grammar: " + errmsg + " in problem " + id)

      //check if we have a word also defined
      val wordStrs = (exerciseElem \ "word").map(_.text)
      val word = if (!wordStrs.isEmpty) {
        //choose the first word and try to parse it as a sentential form
        //of the grammar
        parseWord(wordStrs.toList, refGrammar.get) match {
          case Right(g) => Some(g)
          case Left(error) => throw new IllegalArgumentException(error)
        }
      } else
        None

      //check if an initialGrammar is defined
      //see if we have a reference file
      val initFiles = (exerciseElem \ "initialFile")
      val initGrammar = if (!initFiles.isEmpty) {
        val file = dir + "/" + initFiles(0).text
        val lines = scala.io.Source.fromFile(file).getLines.toList
        val (g, errmsg) = (new GrammarParser()).parseGrammar(lines)
        if (!g.isDefined)
          throw new IllegalArgumentException("Error in parsing initial grammar: " + errmsg + " in problem " + id)
        g
      } else
        None

      new GrammarEntry(id, title, desc, refGrammar.get, word, initGrammar, usecases,
        if (refFiles.isEmpty) None else Some(refFiles(0).text),
        if (initFiles.isEmpty) None else Some(initFiles(0).text))
    })
    //check if all id's are unique    
    if (exers.map(_.id).distinct.size != exers.size) {
      throw new IllegalArgumentException("Ids are not unique")
    }
    exers
  }
}

object GrammarDatabase {
  import ExerciseType._
  val $scope = scala.xml.TopScope
  def readGrammarDatabase(fl: java.io.File, dir: String): GrammarDatabase = {
    object gdb extends ImportGrammarDatabase(fl, dir)
    gdb
  }
  def writeGrammarDatabase(gdb: GrammarDatabase): Unit = {
    val dir = gdb.dir
    val file = gdb.file
    val result = <grammars>{
      gdb.grammarEntries map (ge =>
        <grammar id={ ge.id.toString } usecases={ ge.usecases.mkString(",") }>
          {
            scala.xml.XML.loadString("<title>" + ge.name + "</title>")
          }{
            scala.xml.XML.loadString("<description>" + ge.desc + "</description>")
          }{
            ge.referenceFile match {
              case Some(file) =>
                if (ge.dirtyReferenceFile) {
                  GrammarWriter.dumpGrammar(new File(dir + "/" + file), ge.reference)
                  ge.dirtyReferenceFile = false
                  // Write the initial file back
                }
                <referenceFile>{ file }</referenceFile>
              case None => scala.xml.XML.loadString("<reference>" + ge.reference.rules.map("<rule>" + _ + "</rule>").mkString("\n") + "</reference>")
            }
          }{
            ge.initialFile match {
              case Some(file) =>
                if (ge.dirtyInitialFile) {
                  GrammarWriter.dumpGrammar(new File(dir + "/" + file), ge.initGrammar.get)
                  ge.dirtyInitialFile = false
                  // Write the initial file back
                }
                <initialFile>{ file }</initialFile>
              case None => new xml.NodeBuffer()
            }
          }{
            ge.word match {
              case Some(w) => scala.xml.XML.loadString("<word>" + w.map(_.toString).mkString(" ") + "</word>")
              case None => new xml.NodeBuffer()
            }
          }
        </grammar>: xml.Node)
    }</grammars>
    GrammarWriter.dumpFile(gdb.file, new scala.xml.PrettyPrinter(1000, 2).format(result))
  }
}
