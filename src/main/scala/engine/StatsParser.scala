package engine
import java.io._
import java.text.DecimalFormat
import java.text.SimpleDateFormat
import java.sql.Time
import java.util.Calendar

object StatsParser {

  class StatsInfo(val fn: String) {
    //general stats
    var time = 0.0 //in secs
    var gctime = 0.0 //in secs
    var ratime = 0.0 //in secs
    var rarecur = 0.0
    var nOfwords = 0
    var peakmem = 0
    var timeout = 0 //ms

    //equiv. stats
    var cexs = 0 //counter-examples for equivalence
    var antlrEqCtex = 0
    var cykParseTime = 0.0
    var antlrParseTime = 0.0
    var ctrSize = 0.0 //average size of the counter-example found

    //ambiguity stats
    var ambnts = 0
    var maxWordSizeSearched = 0
    var maxwitsize = 0
  }

  val benchnames = List("C11Grammar1", "C11Grammar2", "SatPaperPascalGrammar", "AntlrPascalGrammar",
    "MozillaJSGrammar", "AntlrJSGrammar", "OracleJavaGrammar", "AntlrJavaGrammar",
    "UCVhdlGrammar", "AntlrVhdlGrammar")
  val benchIds = List("$c1$", "$c2$", "$p1$", "$p2$", "$js1$", "$js2$", "$j1$", "$j2$", "$v1$", "$v2$")

  def main(args: Array[String]) {
    val regrdir = args(0)
    val cfgadir = args(1)
    //regrDriver(dir)
    //computeCFGAStats(dir)
    combinedStats(regrdir, cfgadir)
  }

  def combinedStats(rdir: String, cdir: String) {
    //get stats for every benchmark for every level
    val range = (1 to 10)
    val rlevel1Stats = benchnames.map(bn => (bn -> range.map(i => parseStatsFile(s"$bn-1-$i", rdir)))).toMap
    val rlevel2Stats = benchnames.map(bn => (bn -> range.map(i => parseStatsFile(s"$bn-2-$i", rdir)))).toMap
    val rlevel3Stats = benchnames.map(bn => (bn -> range.map(i => parseStatsFile(s"$bn-3-$i", rdir)))).toMap
    val clevel1Stats = benchnames.map(bn => (bn -> range.map(i => parseCFGAStats(s"$bn-1-$i.cfga-out.txt", cdir)))).toMap
    val clevel2Stats = benchnames.map(bn => (bn -> range.map(i => parseCFGAStats(s"$bn-2-$i.cfga-out.txt", cdir)))).toMap
    val clevel3Stats = benchnames.map(bn => (bn -> range.map(i => parseCFGAStats(s"$bn-3-$i.cfga-out.txt", cdir)))).toMap

    var rtotalCEs = 0
    var ctotalCEs = 0
    var rtime = 0.0
    var ctime = 0.0
    var rcesize = 0.0
    var ccesize = 0.0
    var cdenom = 0
    def statsAsTable(rinfosMap: Map[String, Seq[StatsInfo]], cinfosMap: Map[String, Seq[StatsInfo]]) = {
      var str = ""
      benchnames.zipWithIndex.foreach {
        case (bn, i) =>
          val rinfos = rinfosMap(bn)
          val cinfos = cinfosMap(bn)
          val rinfo = accumulateStats(rinfos)
          val cinfo = accumulateStats(cinfos)
          val line = benchIds(i) + "\t& " +
            rinfo.cexs + "\t& " + cinfo.cexs + "\t& " +
            doubleToStrSuf(rinfo.time, "s") + "\t& " + doubleToStrSuf(cinfo.time, "s") + "\t& " + //average time per benchmark
            doubleToStrSuf(rinfo.ctrSize, "") + "\t&" + doubleToStrSuf(cinfo.ctrSize, "")
          str += line + """\\\hline""" + "\n"
          rtotalCEs += rinfo.cexs
          ctotalCEs += cinfo.cexs
          rtime += rinfo.time
          rcesize += rinfo.ctrSize
          if (!cinfo.time.isNaN()){
            ctime += cinfo.time
            ccesize += cinfo.ctrSize //in this case ctr example, will also be not NAN
            cdenom += 1
          }
      }
      str
    }
    def totalStats() = {
      val rdenom = 3 * benchnames.size      
      rtime /= rdenom
      ctime /= cdenom
      rcesize /= rdenom
      ccesize /= cdenom
      s"""\t& ${rtotalCEs} \t& $ctotalCEs \t& ${doubleToStrSuf(rtime,"s")} \t& ${doubleToStrSuf(ctime,"s")} \t& ${doubleToStr(rcesize)} \t& ${doubleToStr(ccesize)}"""
    }
    
    //first print level1 stats, then level 2, then level 3
    val fields = """Bench. & CExs & CFGA-CExs & time & cfga-time & ctrsize & cfga-ctrsize \\\hline"""
    val str = """\begin{tabular}{|c|c|c|c|c|c|c|}""" + "\n" + fields + "\n" +
      statsAsTable(rlevel1Stats, clevel1Stats) + """\\\hline""" + "\n" +
      statsAsTable(rlevel2Stats, clevel2Stats) + """\\\hline""" + "\n" +
      statsAsTable(rlevel3Stats, clevel3Stats) + """\\\hline""" + "\n" +
      totalStats() + """\\\hline""" + "\n" +
    """\end{tabular}"""

    val pw = new PrintWriter(new FileOutputStream(new File("stats.tex")))
    pw.println(str)
    pw.flush()
    pw.close()
  }

  def simpleDriver(dir: String) {
    //find every file with suffix -amb.stats and -equiv.stats
    var ambStats = List[StatsInfo]()
    var equivStats = List[StatsInfo]()

    (new File(dir)).list().foreach { fn =>
      if (fn.endsWith(".stats")) {
        val statInfo = parseStatsFile(fn, dir)
        if (fn.endsWith("-amb.stats")) {
          ambStats :+= statInfo
        } else
          equivStats :+= statInfo
      }
    }
    //dumpEquivStatsAsTable(equivStats, dir)
    //dumpAmbStatsAsTable(ambStats, dir)
  }

  def regrDriver(dir: String) {
    //get stats for every benchmark for every level
    val range = (1 to 10)
    val level1Stats = benchnames.map(bn => (bn -> range.map(i => parseStatsFile(s"$bn-1-$i", dir)))).toMap
    val level2Stats = benchnames.map(bn => (bn -> range.map(i => parseStatsFile(s"$bn-2-$i", dir)))).toMap
    val level3Stats = benchnames.map(bn => (bn -> range.map(i => parseStatsFile(s"$bn-3-$i", dir)))).toMap

    //first print level1 stats, then level 2, then level 3
    val fields = """Bench. & Ex & T & GC & Words & RAt & RArec & AT & CT & PMem \\\hline"""
    val str = """\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}""" + "\n" + fields + "\n" +
      regrStatsAsTable(level1Stats) + """\\\hline""" + "\n" +
      regrStatsAsTable(level2Stats) + """\\\hline""" + "\n" +
      regrStatsAsTable(level3Stats) + """\\\hline""" + "\n" +
      """\end{tabular}"""
    val pw = new PrintWriter(new FileOutputStream(new File("regr-stats.tex")))
    pw.println(str)
    pw.flush()
    pw.close()
  }

  def parseStatsFile(fn: String, dir: String) = {
    val fis = new FileInputStream(new File(dir + "/" + fn + ".stats"))
    val br = new BufferedReader(new InputStreamReader(fis))
    val statInfo = new StatsInfo(fn)
    var break = false
    while (!break) {
      val lineOrNull = br.readLine()
      if (lineOrNull == null) {
        //reached EOF
        break = true
      } else {
        parseLine(lineOrNull, statInfo)
      }
    }
    //we also need to extract some information from the log file which was unfortunately not tracked in stats
    parseLogFile(fn, dir, statInfo)
    br.close()
    fis.close()
    statInfo
  }

  def parseLine(line: String, statInfo: StatsInfo) = {
    if (line.startsWith("##")) {
      //do nothing
    } else {
      val parts = line.split(":")
      val key = parts(0).trim()
      val value = parts(1).trim()
      if (key == "WordGenCalls") {
        statInfo.nOfwords = value.toInt
      } else if (key == "TimeWithoutGC") {
        statInfo.time = parseTime(value)
      } else if (key == "GCTime") {
        statInfo.gctime = parseTime(value)
      } else if (key == "PeakMemUsageInMB") {
        statInfo.peakmem = value.toInt
      } else if (key == "Avg.RecCalls") {
        statInfo.rarecur = value.toDouble
      } else if (key == "Avg.time-per-call") {
        statInfo.ratime = parseTime(value)
      } else if (key == "Timeout") {
        statInfo.timeout = value.toInt
      } //equiv.stats
      else if (key == "EquivCExs") {
        statInfo.cexs = value.toInt
      } else if (key == "AntlrEquivCExs") {
        statInfo.antlrEqCtex = value.toInt
      } else if (key == "Avg.AntlrParseTime") {
        statInfo.antlrParseTime = parseTime(value)
      } else if (key == "Avg.CYKParseTime") {
        statInfo.cykParseTime = parseTime(value)
      } //ambiguity stats
      else if (key == "AmbWitSizeInc") {
        statInfo.maxWordSizeSearched = value.toInt
      } else if (key == "AmbNts") {
        statInfo.ambnts = value.toInt
      } else if (key == "Max.WitSize") {
        statInfo.maxwitsize = value.toInt
      } else {
        //do nothing
        System.out.println("Field not yer handled: " + key)
      }

    }
  }

  def parseLogFile(fn: String, dir: String, info: StatsInfo) = {
    val fis = new FileInputStream(new File(dir + "/" + fn + ".log"))
    val br = new BufferedReader(new InputStreamReader(fis))
    var break = false
    while (!break) {
      val lineOrNull = br.readLine()
      if (lineOrNull == null) {
        //reached EOF
        break = true
      } else {
        val startString = "Found counter-example of size"
        if (lineOrNull.startsWith(startString)) {
          val ctrsize = lineOrNull.substring(startString.length(), lineOrNull.length()).split(":")(0)
          info.ctrSize = ctrsize.trim().toInt
        }
      }
    }
    br.close()
    fis.close()
  }

  def computeCFGAStats(dir: String) = {

    def CFGAStatsAsTable(infosMap: Map[String, Seq[StatsInfo]]) = {
      var str = ""
      benchnames.zipWithIndex.foreach {
        case (bn, i) =>
          val infos = infosMap(bn)
          val info = accumulateStats(infos)
          str += benchIds(i) + "\t& " + info.cexs + "\t&" + doubleToStr(info.time) + "s" + "\t&" + doubleToStr(info.ctrSize) + """\\\hline""" + "\n"
      }
      str
    }

    //get stats for every benchmark for every level
    val range = (1 to 10)
    val level1Stats = benchnames.map(bn => (bn -> range.map(i => parseCFGAStats(s"$bn-1-$i.cfga-out.txt", dir)))).toMap
    val level2Stats = benchnames.map(bn => (bn -> range.map(i => parseCFGAStats(s"$bn-2-$i.cfga-out.txt", dir)))).toMap
    val level3Stats = benchnames.map(bn => (bn -> range.map(i => parseCFGAStats(s"$bn-3-$i.cfga-out.txt", dir)))).toMap

    //first print level1 stats, then level 2, then level 3
    val fields = """Bench. & Ex & T \\\hline"""
    val str = """\begin{tabular}{|c|c|c|c|}""" + "\n" + fields + "\n" +
      CFGAStatsAsTable(level1Stats) + """\\\hline""" + "\n" +
      CFGAStatsAsTable(level2Stats) + """\\\hline""" + "\n" +
      CFGAStatsAsTable(level3Stats) + """\\\hline""" + "\n" +
      """\end{tabular}"""
    val pw = new PrintWriter(new FileOutputStream(new File("cfga-stats.tex")))
    pw.println(str)
    pw.flush()
    pw.close()
  }

  def parseCFGAStats(fn: String, dir: String) = {
    val statInfo = new StatsInfo(fn)
    val lines = scala.io.Source.fromFile(dir + "/" + fn).getLines.toList
    val cxLine = lines.find(_.startsWith("A word in"))
    if (cxLine.isDefined) {
      statInfo.cexs += 1
      val lastToken = cxLine.get.split(" ").last
      statInfo.ctrSize = lastToken.trim().toInt
    }
    if (lines.last.startsWith("time:")) {
      val timestr = lines.last.substring(5, lines.last.length()) //time is in hrs:min.secs
      val List(mm, secs) = timestr.split(":").toList
      statInfo.time = mm.toInt * 60 + secs.toDouble
    } /*else 
      throw new IllegalStateException(s"Last line of file $fn does not contain time stats")*/
    statInfo
  }

  def parseTime(timestr: String): Double = {
    timestr.dropRight(1).toDouble
  }

  def regrStatsAsTable(infosMap: Map[String, Seq[StatsInfo]]) = {
    var str = ""
    benchnames.zipWithIndex foreach {
      case (bn, i) =>
        val infos = infosMap(bn)
        val info = accumulateStats(infos) //accumulate all the stats for this benchmark
        val line = benchIds(i) + "\t& " +
          info.cexs + "\t& " +
          doubleToStr(info.time) + "s\t& " + //average time per benchmark
          doubleToStr(info.ctrSize) + "\t&" +
          //doubleToStr(info.gctime) + "s\t& " + 
          getNumWords(info.nOfwords) + "\t& " + //number of words in millions
          doubleToStr(info.ratime * 1000) + "ms" //+ "\t& " +        
        //doubleToStr(info.rarecur) + "\t& " + 
        //doubleToStr(info.antlrParseTime * 1000) + "ms" \t& " +
        //doubleToStr(info.cykParseTime) + "s\t& " + 
        //doubleToStr(info.peakmem /1000.0) +"GB"
        str += line + """\\\hline""" + "\n"
    }
    str
  }

  def getNumWords(numwords: Int) = {
    val million = (1000 * 1000)
    if (numwords < million)
      doubleToStr((numwords / 1000.0)) + "K"
    else
      doubleToStr((numwords.toDouble / million.toDouble)) + "M"
  }

  def accumulateStats(infos: Seq[StatsInfo]): StatsInfo = {
    val posinfos = infos.filter(_.cexs > 0) //those cases where a counter-example was found
    val size = infos.size
    val possize = posinfos.size
    var cumInfo = new StatsInfo("")
    cumInfo.time = posinfos.map(_.time).sum / possize
    cumInfo.gctime = infos.map(_.gctime).sum / size
    cumInfo.ratime = infos.map(_.ratime).sum / size
    cumInfo.rarecur = infos.map(_.rarecur).sum / size
    cumInfo.nOfwords = infos.map(_.nOfwords).sum
    cumInfo.peakmem = infos.map(_.peakmem).max
    cumInfo.cexs = infos.map(_.cexs).sum
    cumInfo.antlrEqCtex = infos.map(_.antlrEqCtex).sum
    cumInfo.cykParseTime = infos.map(_.cykParseTime).sum / size
    cumInfo.antlrParseTime = infos.map(_.antlrParseTime).sum / size
    cumInfo.ctrSize = infos.map(_.ctrSize).sum / possize //average counter-example size
    cumInfo
  }

  def doubleToStrSuf(d: Double, sf: String): String = {
    if (d.isNaN()) {
      "-"
    } else
      String.format("%.1f", d.asInstanceOf[Object]) + sf
  }

  def doubleToStr(d: Double): String = {
    String.format("%.1f", d.asInstanceOf[Object])
  }

  def dumpEquivStatsAsTable(infos: List[StatsInfo], dir: String) = {
    var str = ""
    infos.foreach { info =>
      val line = info.fn + "\t& " + info.cexs + "\t& " +
        String.format("%.1f", info.time.asInstanceOf[Object]) + "s" + "\t& " + info.nOfwords + "\t& " +
        String.format("%.1f", (info.ratime * 1000).asInstanceOf[Object]) + "ms" + "\t& " +
        String.format("%.1f", info.rarecur.asInstanceOf[Object])
      str += line + "\\\\\n"
    }
    val pw = new PrintWriter(new FileOutputStream(new File(dir + "/" + "equiv-stats.txt")))
    pw.println(str)
    pw.flush()
    pw.close()
  }

  def dumpAmbStatsAsTable(infos: List[StatsInfo], dir: String) = {

    def nwToString(nw: Int) = {
      if (nw < 100000) {
        nw.toString
      } else {
        Math.round((nw / (1000 * 1000))).toString + "M"
      }
    }
    def timeToStr(info: StatsInfo) = {
      if (info.timeout > 0)
        "-"
      else
        String.format("%.1f", info.time.asInstanceOf[Object]) + "s"
    }

    var str = ""
    infos.foreach { info =>
      val line = info.fn + "\t& " + info.ambnts + "\t& " + info.maxwitsize + "\t& " +
        info.maxWordSizeSearched + "\t& " +
        timeToStr(info) + "\t& " + nwToString(info.nOfwords) + "\t& " +
        String.format("%.1f", (info.ratime * 1000 * 1000).asInstanceOf[Object]) + "$\\mu$s" + "\t& " +
        String.format("%.1f", info.rarecur.asInstanceOf[Object])
      str += line + "\\\\\n"
    }
    val pw = new PrintWriter(new FileOutputStream(new File(dir + "/" + "amb-stats.txt")))
    pw.println(str)
    pw.flush()
    pw.close()
  }
}