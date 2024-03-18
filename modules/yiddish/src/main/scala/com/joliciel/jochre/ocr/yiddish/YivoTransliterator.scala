package com.joliciel.jochre.ocr.yiddish

import org.rogach.scallop._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.SeqMap
import scala.collection.compat.immutable.ArraySeq
import scala.io.Source
import scala.util.matching.Regex

/** Ported from Python to Scala. Origin: https://github.com/ibleaman/yiddish
  */
object YivoTransliterator {

  private val precombinedUnicodePairs = SeqMap(
    ("וּ", "וּ"),
    ("יִ", "יִ"),
    ("ײַ", "ײַ"),
    ("וו", "װ"),
    ("וי", "ױ"),
    ("יי", "ײ"),
    ("אַ", "אַ"),
    ("אָ", "אָ"),
    ("בֿ", "בֿ"),
    ("כּ", "כּ"),
    ("פּ", "פּ"),
    ("פֿ", "פֿ"),
    ("שׂ", "שׂ"),
    ("תּ", "תּ")
  )

  private val pairsWithoutVovYud =
    precombinedUnicodePairs.removedAll(Seq("װ", "ױ", "ײ"))

  private def replaceAll(map: Seq[(String, String)], string: String): String =
    map.foldLeft(string) { case (string, (find, replace)) =>
      string.replace(find, replace)
    }

  private def replaceAllRegex(
      map: Seq[(Regex, String)],
      string: String
  ): String =
    map.foldLeft(string) { case (string, (find, replace)) =>
      find.replaceAllIn(string, replace)
    }

  def replaceWithPrecombined(string: String): String =
    replaceAll(precombinedUnicodePairs.toSeq, string)
      .replace("בּ", "ב")
      .replace("בּ", "ב")

  // When vovYud==True, these will be preserved as precombined chars:
  //      װ, ײ, ױ
  private def replaceWithDecomposed(string: String, vovYud: Boolean = false): String = {
    val pairsToReplace = if (vovYud) {
      pairsWithoutVovYud
    } else {
      precombinedUnicodePairs
    }
    replaceAll(pairsToReplace.toSeq, string)
      .replace("ייַ", "ײַ")
      .replace("בּ", "ב")
      .replace("בּ", "ב")
  }

  private def replacePunctuation(string: String): String = {
    string
      .replace("-", "־")
      .replace("′", "\"")
      .replace("׳", "\"")
      .replace(
        "″",
        """)
      .replace("״", """
      )
  }

  private val diacritics = """[ִַַָּּּּֿֿׂ]""".r

  def stripDiactritics(string: String): String = { // and replace with decomposed
    diacritics.replaceAllIn(replaceWithDecomposed(string), "")
  }

  private val loshnKoydeshPronunciationList =
    Source.fromResource("yiddish/orthographic-to-phonetic.txt").getLines()

  private val loshnKoydeshPronunciationMap: Map[String, Seq[String]] =
    loshnKoydeshPronunciationList
      .map(_.split("\t"))
      .map(_.map(replaceWithPrecombined).map(replacePunctuation))
      .map(words => words(0) -> words(1))
      .toMap
      .view
      .mapValues(_.split(",").toSeq)
      .toMap

  private val reverseLoshnKoydeshPronunciationMap: Map[String, String] =
    loshnKoydeshPronunciationMap
      .flatMap { case (key, entries) => entries.map(_ -> key).toMap }

  private val germanicSemiticHomographs = Set(
    "אין",
    "צום",
    "בין",
    "ברי",
    "מיד",
    "קין",
    "שער",
    "מעגן",
    "צו",
    "מאַנס",
    "טוען",
    "מערער"
  )

  private val lessCommonLoshnKoydeshPronunciations = Set(
    "אַדױשעם",
    "כאַנוקע",
    "גדױלע",
    "כאַװײרע",
    "מיכיע",
    "כאָװער",
    "אָרעװ",
    "מאָסער",
    "כיִעס",
    "זקאָנים",
    "נעװאָלע",
    "מאַשלעם",
    "כפֿאָצים",
    "כאַכאָמע",
    "טאַנאָיִם",
    "יאָסעף",
    "יאָסעפֿס",
    "יאָסעפֿן"
  )

  private val transliterationTable = Seq( // all are precombined
    ("א", ""),
    ("אַ", "a"),
    ("אָ", "o"),
    ("ב", "b"),
    ("בֿ", "v"),
    ("ג", "g"),
    ("דזש", "dzh"),
    // ("דז", "dz"), // phonemic status doubtful
    ("ד", "d"),
    ("ה", "h"),
    ("ו", "u"),
    ("וּ", "u"),
    ("װ", "v"),
    ("ױ", "oy"),
    ("זש", "zh"),
    ("ז", "z"),
    ("ח", "kh"),
    ("טש", "tsh"),
    ("ט", "t"),
    ("י", "j"),
    ("יִ", "i"),
    ("ײ", "ey"),
    ("ײַ", "ay"),
    ("כ", "kh"),
    ("כּ", "k"),
    ("ך", "kh"),
    ("ל", "l"),
    ("מ", "m"),
    ("ם", "m"),
    ("נ", "n"),
    ("ן", "n"),
    ("ס", "s"),
    ("ע", "e"),
    ("פּ", "p"),
    ("פֿ", "f"),
    ("פ", "f"),
    ("ף", "f"),
    ("צ", "ts"),
    ("ץ", "ts"),
    ("ק", "k"),
    ("ר", "r"),
    ("ש", "sh"),
    ("שׂ", "s"),
    ("תּ", "t"),
    ("ת", "s"),
    ("־", "-")
  )

  private val yiddishTokenizer =
    """[אאַאָבבֿגדהוװוּױזחטייִײײַככּךלמםנןסעפּפֿףצץקרששׂתּת\-־"]+|[^אאַאָבבֿגדהוװוּױזחטייִײײַככּךלמםנןסעפּפֿףצץקרששׂתּת\-־"]""".r

  // if loshnKoydesh, look up string in LK dictionary
  def transliterate(string: String, loshnKoydesh: Boolean = true): String = {
    val precombined = replaceWithPrecombined(string)

    val withLoshnKoydesh = if (loshnKoydesh) {
      val tokens = yiddishTokenizer.findAllIn(precombined)
      val newTokens = tokens.map { token =>
        if (
          loshnKoydeshPronunciationMap.contains(
            token
          ) && !germanicSemiticHomographs.contains(token)
        ) {
          if (
            lessCommonLoshnKoydeshPronunciations.contains(
              loshnKoydeshPronunciationMap(token).head
            ) && loshnKoydeshPronunciationMap(token).size > 1
          ) {
            loshnKoydeshPronunciationMap(token)(1).replace("־", "-")
          } else {
            loshnKoydeshPronunciationMap(token).head.replace("־", "-")
          }
        } else {
          token
        }
      }
      newTokens.mkString("")
    } else {
      precombined
    }

    replaceAll(transliterationTable, withLoshnKoydesh)
      .replaceAll("j$", "i")
      .replaceAll("j(?![aeiou])", "i")
      .replace("j", "y")
  }

  private val reverseTransliterationRegex: Seq[(Regex, String)] = Seq(
    ("""\bay""".r, "אײַ"),
    ("""\bey""".r, "אײ"),
    ("""\boy""".r, "אױ"),
    ("""\bu""".r, "או"),
    ("""\bi""".r, "אי"),
    ("""kh\b""".r, "ך"),
    ("""m\b""".r, "ם"),
    ("""n\b""".r, "ן"),
    ("""f\b""".r, "ף"),
    ("""ts\b""".r, "ץ")
  )

  private val reverseTransliterationTable: Seq[(String, String)] =
    Seq( // to precombined
      ("ayi", "ײַיִ"), // מאַלײַיִש
      ("eyi", "ײיִ"), // פּאַרטײיִש, שנײיִק
      ("oyi", "ױיִ"), // פֿרױיִש
      ("ay", "ײַ"),
      ("ey", "ײ"),
      ("oy", "ױ"),
      ("zh", "זש"),
      ("kh", "כ"),
      ("sh", "ש"), // דײַטש, *דײַצה
      ("ts", "צ"),
      ("ia", "יִאַ"), // ?
      ("ai", "אַיִ"), // יודאַיִסטיק
      ("ie", "יִע"), // פֿריִער, בליִען, קיִעװ
      ("ei", "עיִ"), // העברעיִש, פֿעיִק
      ("ii", "יִיִ"), // װאַריִיִרן, פֿריִיִק, אַליִיִרט
      ("io", "יִאָ"), // טריִאָ
      ("oi", "אָיִ"), // דאָיִק
      ("iu", "יִו"), // בליִונג, באַציִונג
      ("ui", "ויִ"), // גראַדויִר
      ("iyi", "יִייִ"), // ?
      ("yi", "ייִ"),
      ("iy", "יִי"), // ?
      ("uvu", "וּװוּ"), // פּרוּװוּנג, צוּװוּקס
      ("uv", "וּװ"),
      ("vu", "װוּ"),
      ("uu", "וּו"), // טוּונג, דוּומװיראַט
      ("uy", "וּי"), // בורזשוּי
      ("a", "אַ"),
      ("b", "ב"),
      ("d", "ד"),
      ("e", "ע"),
      ("f", "פֿ"),
      ("g", "ג"),
      ("h", "ה"),
      ("i", "י"),
      ("k", "ק"),
      ("l", "ל"),
      ("m", "מ"),
      ("n", "נ"),
      ("o", "אָ"),
      ("p", "פּ"),
      ("r", "ר"),
      ("s", "ס"),
      ("t", "ט"),
      ("u", "ו"),
      ("v", "װ"),
      ("y", "י"),
      ("z", "ז")
    )

  private val reversTransliterationFinalRegex: Seq[(Regex, String)] = Seq(
    ("""ך(“|")""".r, "כ$1"), // fix mistakes: for abbreviations/acronyms
    ("""ם(“|")""".r, "מ$1"),
    ("""ן(“|")""".r, "נ$1"),
    ("""ף(“|")""".r, "פֿ$1"),
    ("""ץ(“|")""".r, "צ$1"),
    ("""\bך""".r, "כ"), // no word-initial final letters
    ("""\bם""".r, "מ"),
    ("""\bן""".r, "נ"),
    ("""\bף""".r, "פֿ"),
    ("""\bץ""".r, "צ")
  )

  private val reverseTransliterationExceptions: Seq[(Regex, String)] = Seq(
    // unpredicted shtumer alef
    ("""\bfarey""".r, "פֿאַראײ"), // פֿאַראײניקט, פֿאַראײביקן
    ("""\bantiintel""".r, "אַנטיאינטעל"), // אַנטיאינטעלעקטואַליזם
    ("""\bbizitst""".r, "ביזאיצט"), // ביזאיצטיקער
    ("""\boybnoy""".r, "אױבנאױ"), // אױבנאױף
    ("""\boysib""".r, "אױסאיב"), // אױסאיבן
    ("""geibt""".r, "געאיבט"),
    ("""geiblt""".r, "געאיבלט"),
    ("""tsuibn\b""".r, "צואיבן"),
    ("""\boyseydl""".r, "אױסאײדל"), // אױסאײדלען
    ("""geeydl""".r, "געאײדל"),
    ("""tsueydl""".r, "צואײדל"),
    ("""\bayneyg""".r, "אײַנאײג"), // אײַנאײגענען
    ("""geey""".r, "געאײ"),
    ("""tsuey""".r, "צואײ"),
    ("""geindlt""".r, "געאינדלט"), // surfing
    ("""\bumoys""".r, "אומאױס"), // אומאױסשעפּלעך
    ("""\bumayn""".r, "אומאײַנ"), // אומאײַנגענעם
    ("""\bumeydl""".r, "אומאײדל"), // אומאײדל
    ("""\bumeydel""".r, "אומאײדעל"), // אומאײדעלע
    ("""\bureynikl""".r, "אוראײניקל"),
    ("""\bbaayn""".r, "באַאײַנ"), // באַאײַנדרוקן, באַאײַנפֿלוסן
    ("""geayn""".r, "געאײַנ"), // געאײַנפֿלוסט
    ("""tsuayn""".r, "צואײַנ"),
    ("""durkhayl""".r, "דורכאײַל"), // דורכאײַלן
    ("""farbayayl""".r, "פֿאַרבײַאײַל"), // דורכאײַלן
    ("""geay""".r, "געאײַ"),
    ("""tsuayl""".r, "צואײַל"), // געאײַנפֿלוסט
    ("""geirtst""".r, "געאירצט"),
    ("""tsuirtsn\b""".r, "צואירצן"),
    ("""grobayz""".r, "גראָבאײַז"), // גראָבאײַזנס
    ("""presayz""".r, "פּרעסאײַז"),
    ("""halbindzl""".r, "האַלבאינדזל"),
    ("""hinteroyg""".r, "הינטעראױג"), // הינטעראױגיק
    ("""zunoyfgang""".r, "זונאױפֿגאַנג"),
    ("""moyleyzl""".r, "מױלאײזל"),
    ("""\bfarum""".r, "פֿאַראומ"), // פֿאַראומװערדיקן, פֿאַראומעטיקטע, פֿאַראומרײניקן
    ("""\bfarur""".r, "פֿאַראור"), // פֿאַראַורטײל
    ("""\bforur""".r, "פֿאָראור"), // פֿאָראורטל
    ("""\bfaribl""".r, "פֿאַראיבל"),
    ("""\bfarinteres""".r, "פֿאַראינטערעס"), // פֿאַראינטערעסירן

    // ay != ײַ
    ("""\brayon\b""".r, "ראַיאָן"),
    ("""\brayonen\b""".r, "ראַיאָנען"),
    ("""bayornt""".r, "באַיאָרנט"),
    ("""bayort""".r, "באַיאָרט"),
    ("""mayontik""".r, "מאַיאָנטיק"),
    ("""mayontkes""".r, "מאַיאָנטקעס"),
    ("""mayonez""".r, "מאַיאָנעז"),
    ("""mayestet""".r, "מאַיעסטעט"),
    ("""payats\b""".r, "פּאַיאַץ"),
    ("""payatsn\b""".r, "פּאַיאַצן"),
    ("""payatseve""".r, "פּאַיאַצעװע"),
    ("""farayorik""".r, "פֿאַראַיאָריק"),
    ("""\bkayor""".r, "קאַיאָר"),
    ("""\bayed""".r, "אַיעד"), // אַיעדער
    ("""\bayo\b""".r, "אַיאָ"),

    // ey != ײ
    ("""geyogt""".r, "געיאָגט"),
    ("""geyeg""".r, "געיעג"),
    ("""\bgeyog\b""".r, "געיאָג"),
    ("""geyavet""".r, "געיאַװעט"),
    ("""geyadet""".r, "געיאַדעט"),
    ("""geyopet""".r, "געיאָפּעט"),
    ("""geyabede""".r, "געיאַבעדע"), // געיאַבעדע(װע)ט
    ("""geyakhmert""".r, "געיאַכמערט"),
    ("""tseyakhmert""".r, "צעיאַכמערט"),
    ("""tseyakhmet""".r, "צעיאַכמעט"),
    ("""geyodlt""".r, "געיאָדלט"),
    ("""geyomer""".r, "געיאָמער"),
    ("""tseyomer""".r, "צעיאָמער"),
    ("""geyutshet""".r, "געיוטשעט"),
    ("""geyoyr""".r, "געיױר"), // געיױרענע
    ("""\bgeyet(\b|er|e|n|s|ns)""".r, "געיעט$1"),
    ("""geyentst""".r, "געיענצט"),
    ("""geyenket""".r, "געיענקעט"),
    ("""geyekt""".r, "געיעקט"),
    ("""\bgeyert\b""".r, "געיערט"),
    ("""pleyade""".r, "פּלעיאַדע"),

    // oy != ױ
    ("""proyekt""".r, "פּראָיעקט"), // פּראָיעקטאָר
    ("""umloyal""".r, "אומלאָיאַל"),
    ("""loyal""".r, "לאָיאַל"),
    ("""paranoye""".r, "פּאַראַנאָיע"),

    // ts != צ
    ("""tstu\b""".r, "טסטו"),
    ("""\beltst""".r, "עלטסט"),
    ("""\bkeltst""".r, "קעלטסט"),
    ("""\bbalibtst""".r, "באַליבטסט"),
    ("""\bgeburts""".r, "געבורטס"),
    ("""\barbets""".r, "אַרבעטס"),
    ("""\barbayts""".r, "אַרבײַטס"),
    ("""\bdemolts""".r, "דעמאָלטס"),
    ("""\bgots""".r, "גאָטס"),
    ("""\bguts""".r, "גוטס"),
    ("""\bgeshefts""".r, "געשעפֿטס"),
    ("""(\b|ba|far|der)haltst""".r, "$1האַלטסט"),
    ("""\bshlekhts\b""".r, "שלעכטס"),
    ("""(\b|tse)shpaltst""".r, "$1שפּאַלטסט"),
    ("""(\b|tse|far)shpreytst""".r, "$1שפּרײטסט"),
    ("""shpetst""".r, "שפּעטסט"),
    ("""\brekhts\b""".r, "רעכטס"),
    ("""du shatst""".r, "דו שאַטסט"), // cf. ער שאַצט
    ("""\bforverts\b""".r, "פֿאָרװערטס"),

    // kh != כ
    ("""\bpikhol""".r, "פּיקהאָל"), // פּיקהאָלץ, פּיקהאָלצן
    ("""\btsurikhalt""".r, "צוריקהאַלט"), // צוריקהאַלטן etc.
    ("""\bkrikhalt""".r, "קריקהאַלט"),

    // sh != ש
    (
      """\boysh(?!ers?\b|vits(er)?\b)""".r,
      "אױסה"
    ), // the only exceptions to oysh = אױסה {
    // עושר, עושרס, אױשװיץ, אױשװיצער
    ("""\baroysh""".r, "אַרױסה")
  )

  private val semiticGermanicHomophones: Set[String] = Set(
    "אָדער",
    "אױפֿן",
    "איבער",
    "אײן",
    "אים",
    "בױ",
    "דאַן",
    "װײס",
    "װעסט",
    "זאָל",
    "טאָמער",
    "טו",
    "לײען",
    "מאָגן",
    "מאַן",
    "מוטער",
    "מײַנע",
    "מע",
    "נעמען",
    "עמער",
    "פּױלן",
    "קעלער",
    "קעץ",
    "שװאַך",
    "שיִער",
    "שנײ"
  )

  private val tokenizerRegex: Regex = """(?U)[\w\-־]+|[^\w\-־]""".r

  // note: output uses precombined Unicode characters
  // if loshnKoydesh, look up string in LK dictionary
  def detransliterate(string: String, loshnKoydesh: Boolean = true): String = {
    val string1 =
      replaceAllRegex(reverseTransliterationExceptions, string.toLowerCase)
    val string2 = replaceAllRegex(reverseTransliterationRegex, string1)
    val string3 = replaceAll(reverseTransliterationTable, string2)
    val string4 = replaceAllRegex(reversTransliterationFinalRegex, string3)

    val string5 = if (loshnKoydesh) {
      val newTokens = tokenizerRegex.findAllIn(string4).map { token =>
        if (
          reverseLoshnKoydeshPronunciationMap.contains(
            token.replace('-', '־')
          ) && !semiticGermanicHomophones.contains(token)
        ) {
          reverseLoshnKoydeshPronunciationMap(token.replace('-', '־'))
            .replace('־', '-')
        } else {
          token
        }
      }
      newTokens.mkString("")
    } else {
      string4
    }
    string5
  }

  private class CLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val input: ScallopOption[String] = opt[String](required = true)
    val output: ScallopOption[String] = opt[String](required = true)

    verify()
  }

  val textSimplifier: YiddishTextSimpifier = YiddishTextSimpifier(true)

  def main(args: Array[String]): Unit = {
    val cli = new CLI(ArraySeq.unsafeWrapArray(args))
    val inputPath = Path.of(cli.input())
    val outputPath = Path.of(cli.output())
    outputPath.toFile.getParentFile.mkdirs()

    val withDelimiter = "(?U)((?<=%1$s)|(?=%1$s))"
    val whiteSpaceAndPunct = String.format(withDelimiter, raw"(\s+)|(\p{Punct})")

    val lines = Source.fromFile(inputPath.toFile).getLines()
    val transliterated = lines.map { line =>
      val words = line.split(whiteSpaceAndPunct)
      val transliterated = words.map(transliterate(_))
      transliterated.mkString
    }

    Files.write(
      outputPath,
      transliterated.mkString("\n").getBytes(StandardCharsets.UTF_8)
    )
  }
}
