package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.utils.ImageUtils
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory

import java.io.File
import javax.imageio.ImageIO
import scala.xml.XML

class AltoTest extends AnyFlatSpec with Matchers with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  "processedText" should "correctly convert Alto to text" in {
    val altoFile = {
      <Page>
        <PrintSpace>
        <TextBlock>
          <TextLine>
            <String CONTENT="Jimi" /><SP /><String CONTENT="Hend" SUBS_CONTENT="Hendrix"/><HYP CONTENT="-" />
          </TextLine>
          <TextLine>
            <String CONTENT="rix" SUBS_CONTENT="Hendrix" /><SP /><String CONTENT="Experience"/>
          </TextLine>
        </TextBlock>
        </PrintSpace>
      </Page>
    }

    val page = Page.fromXML(altoFile)
    page.processedContent shouldEqual "Jimi Hendrix Experience"
  }

  "allTextBlocks" should "list text boxes correctly" in {
    // Testing Alto that caused error in previous version of allTextBlocks
    val altoXml = XML.load(getClass.getResourceAsStream("/alto/nybc201086_0022_alto4.xml"))
    val alto = Alto.fromXML(altoXml)
    val page = alto.pages.head

    if (log.isDebugEnabled) {
      // Write page to temp directory to view annotated contents
      val image = ImageIO.read(getClass.getResourceAsStream("/images/nybc201086_0022.jpg"))
      val mat = toRGB(fromBufferedImage(image))

      page.draw(mat)
      val tempFile = File.createTempFile("jochre", ".jpg").toPath
      log.debug(f"temp file: $tempFile")

      saveImage(
        mat,
        tempFile
      )

      tempFile.toFile.delete()
    }

    val page22Blocks = page.allTextBlocks

    page22Blocks.size shouldEqual 79
  }
}
