
import java.awt.{Color, Font}
import java.io.File
import javax.imageio.ImageIO

import org.imgscalr.Scalr
import org.scalatest.FunSuite

import scala.swing.Font
import scala.util.Random

/**
  * Created by mtao60 on 4/29/17.
  */
class SliceImage$Test extends FunSuite {

  test("testCrop") {

    val image = SliceImage.loadImage("/Users/mtao60/Documents/test/bgd.png")
    val newImage = SliceImage.crop(image, 0, 1000, 0, 1000)
    ImageIO.write(newImage, "png", new File("/tmp/test.png"))
  }


  test("testPad") {
    val image = SliceImage.loadImage("/Users/mtao60/Documents/test/bgd.png")
    val newImage = SliceImage.crop(image, 0, 1000, 0, 1000)
    val paddedImage = SliceImage.pad(newImage, 100, 200, 300, 400)
    ImageIO.write(paddedImage, "png", new File("/tmp/paedTest.png"))
  }

  test("testWriteWord") {
    val image = SliceImage.creatBlankImage(400, 500)
    SliceImage.writeWord(image, "Hello World", 20, 50, new Font("TimesRoman", Font.PLAIN, 9))
    ImageIO.write(image, "png", new File("/tmp/stringImage.png"))
  }

  test("testSlice") {
    var max = 0

    var tSize = 0
    val gap = 10
    var min = 0

    var re = SliceImage.slice(100, 1, 0)
    assert(re(0)._1 == 0)
    assert(re(0)._2 == 100)

    //

    tSize = 2
    re = SliceImage.slice(100, tSize, 0)

    assert(re(0)._1 == 0)
    assert(re(tSize - 1)._2 + re(tSize - 1)._1 == 100)
    assert(re(0)._2 == re(tSize - 1)._1)


    tSize = 2
    re = SliceImage.slice(100, tSize, gap).map(a => (a._1, a._1 + a._2))

    assert(re(0)._1 == 0)
    assert(re(tSize - 1)._2 == 100)
    assert(re(0)._2 - re(tSize - 1)._1 < gap + 1)
    assert(re(0)._2 - re(tSize - 1)._1 >= gap - 1)


    for (tSize <- 3 to 10) {

      val totla = 1000 + Random.nextInt(500)

      println(s"total:${totla} tSize=${tSize}")
      re = SliceImage.slice(totla, tSize, gap).map(a => (a._1, a._1 + a._2))

      re.foreach(a => {
        println(s"${a._1}  ${a._2}")
      })
      assert(re(0)._1 == 0)
      assert(re(tSize - 1)._2 == totla)

      val t = (1 until tSize).map(i => re(i - 1)._2 - re(i)._1)

      val max = t.foldLeft(Int.MinValue)(math.max(_, _))
      val min = t.foldLeft(Int.MaxValue)(math.min(_, _))

      assert(math.abs(max - gap) < 2)
      assert(math.abs(min - gap) < 2)


    }


  }

  test("testFloot") {
    assert(math.rint(0.3) == 0)
    assert(math.rint(0.5) == 1)
  }

  test("testgetBestSliceSize") {
    println(SliceImage.getBestSliceSize(10000, 2000, 50))
  }

  test("testMain") {

    //SliceImage.main(Array("-i","/Users/mtao60/Documents/test/bgd.png","-o","/tmp/tt","--test"))
    //    SliceImage.main(Array("-i","/Users/mtao60/Documents/test/bgd.png",
    //      "-o","/tmp/tt",
    //      "--image-pad","0",
    //      "--footer-height","250"
    //    ,"--image-pad-bottom","100"
    //    //,"--test"
    //    ))
    SliceImage.main(Array("-i", "/Users/mtao60/Documents/test/bgd.png",
      "-o", "/tmp/tt",
      "--image-pad", "100",
      "--footer-height", "250"
      , "--image-pad-bottom", "100"
      , "--new-image-width", "" + math.rint(8.27 * 600).toInt
      , "--new-image-height", "" + math.rint(11.69 * 600).toInt
      //, "--test"
    ))

  }

  test("testfillString") {
    val text = "Hellow World"
    val (img1, f1) = SliceImage.fillString(1000, 100, 10, text, Color.RED, Color.WHITE, new Font("TimesRoman", Font.PLAIN, 1))

    println(s"f1 size:${f1.getSize}")
    ImageIO.write(img1, "png", new File("/tmp/tt/f1.png"))

    val (img2, f2) = SliceImage.fillString(1000, 100, 10, text, Color.RED, Color.WHITE, new Font("TimesRoman", Font.PLAIN, 10000))

    println(s"f2 size:${f2.getSize}")
    ImageIO.write(img2, "png", new File("/tmp/tt/f2.png"))

  }


  def printMetrix(metrix: Array[Array[(String, Int, Int, Boolean)]]) = {
    metrix.foreach(a => {
      println(a.mkString(";"))
    })

    println("============")
  }

  test("testcreateNeighborsMetrix") {
    var metrix = SliceImage.createNeighborsMetrix(0, 0, 1, 1)

    printMetrix(metrix)

    metrix = SliceImage.createNeighborsMetrix(0, 0, 2, 1)

    printMetrix(metrix)
    metrix = SliceImage.createNeighborsMetrix(0, 0, 1, 2)

    printMetrix(metrix)
    metrix = SliceImage.createNeighborsMetrix(0, 0, 2, 2)

    printMetrix(metrix)

    metrix = SliceImage.createNeighborsMetrix(1, 1, 2, 2)

    printMetrix(metrix)

    (0 to 5).foreach(y => {
      (0 to 4).foreach(x => {
        println(s"x:${x} y:${y}")
        metrix = SliceImage.createNeighborsMetrix(x, y, 5, 6)

        printMetrix(metrix)
      })
    })

  }

  test("testcreateNeighbors") {
    (0 to 5).foreach(y => {
      (0 to 4).foreach(x => {
        println(s"x:${x} y:${y}")
        val img = SliceImage.createNeighbors(400, 400, x, y, 5, 6)

        ImageIO.write(img, "png", new File(s"/tmp/tt/n_${x}_${y}.png"))
      })
    })
  }
}
