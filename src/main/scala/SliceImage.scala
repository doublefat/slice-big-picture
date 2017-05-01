import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.awt._
import java.awt.image.BufferedImage
import java.io.{File, FileInputStream}
import javax.imageio.ImageIO

import com.aol.advertising.ml.plib.commandline_parameters.{Parameter, ParameterParser}
import com.aol.advertising.ml.plib.utils.LocalFileSystem
import org.imgscalr.Scalr


/**
  * Created by mtao60 on 4/29/17.
  */
object SliceImage {

  var defaultFontName="TimesRoman"
  val xChars=(1 to 26).map(i=>('A'.toInt+i-1).toChar).map(""+_).toArray
  val yChars=(1 to 26).map(i=>('1'.toInt+i-1).toChar).map(""+_).toArray

  def getBestSliceSize(total: Int, max: Int,over:Int):(Int,Int) = {
    if(max>total){
      (1,max)
    }
    else{

      var s=Int.MaxValue
      var i=0
      while(s>max && i<1000){
        i+=1
        s=slice(total,i,over)(0)._2

      }


      if(i <1000){
        (i,s)
      }
      else{
        throw new IllegalArgumentException(s"Could not find slice size ${max} in ${total}")
      }
    }
  }

  def createColumnRowReadme(width: Int, height: Int, column: Int, row: Int) = {
    val readme=creatBlankImage(width,height,Color.WHITE)
    val g=readme.getGraphics.asInstanceOf[Graphics2D]

    val pad=30
    val margin=10

    val maxW=width-pad*2
    val maxH=height-pad*2

    val xMessage=s"X: (${(0 until column).map(xChars(_)).mkString(", ")});"
    val yMessage=s"Y: (${(0 until row).map(yChars(_)).mkString(", ")});"

    var c=0
    var found=false
    val fontName=defaultFontName
    println(s"${column} x:${xMessage}")
    println(s"${row} y:${yMessage}")
    println(s"x max:${maxW}")
    println(s"y max:${maxH}")
    while(c<1000 && !found){
      c+=1
      val xFont=getStringSize2(xMessage,new Font(fontName, Font.PLAIN, c),g)
      val yFont=getStringSize2(yMessage,new Font(fontName, Font.PLAIN, c),g)

//      println(s"font size:${c}")
//      println(s"x font :${xFont}")
//      println(s"u font :${yFont}")

      if(math.max(xFont._1,yFont._1) >maxW || xFont._2+yFont._2+margin>maxH ){
        //println("Found!  ")
        found=true
      }


    }

    val fontSize=c-1
    val font=new Font(fontName, Font.PLAIN, fontSize)
    val xFont=getStringSize2(xMessage,font,g)
    val yFont=getStringSize2(yMessage,font,g)

//    println(s"Using xfond:${xFont}")
//    println(s"Using yfond:${yFont}")

    g.setFont(font)
    g.setColor(Color.BLACK)
    g.drawString(xMessage,pad,pad+xFont._2)
    g.drawString(yMessage,pad,pad+xFont._2+margin+yFont._2)


//
    //ImageIO.write(readme,"png",new File("/tmp/tt/t.png"))

    readme
  }

  def fillMetrix(metrix: Array[Array[(String, Int, Int, Boolean)]], x: Int, y: Int,maxX:Int,maxY:Int):Unit = {
    val me=metrix(y)(x)
    if(x-1>=0){
      val newX=x-1
      val newColumn=me._2-1
      val newRow=me._3
      if(metrix(y)(newX)==null){
        metrix(y)(newX)= (""+xChars(newColumn)+yChars(newRow),newColumn,newRow,false)
        fillMetrix(metrix,newX,y,maxX,maxY)
      }
    }
    if(x+1<maxX){
      val newX=x+1
      val newColumn=me._2+1
      val newRow=me._3
      if(metrix(y)(newX)==null){
        metrix(y)(newX)= (""+xChars(newColumn)+yChars(newRow),newColumn,newRow,false)
        fillMetrix(metrix,x+1,y,maxX,maxY)
      }
    }

    if(y-1>=0){
      val newY=y-1
      val newRow=me._3-1
      val newColumn=me._2
      if(metrix(newY)(x)==null){
        metrix(newY)(x)= (""+xChars(newColumn)+yChars(newRow),newColumn,newRow,false)
        fillMetrix(metrix,x,newY,maxX,maxY)
      }
    }
    if(y+1<maxY){
      val newY=y+1
      val newRow=me._3+1
      val newColumn=me._2
      if(metrix(newY)(x)==null){
        metrix(newY)(x)= (""+xChars(newColumn)+yChars(newRow),newColumn,newRow,false)
        fillMetrix(metrix,x,newY,maxX,maxY)
      }
    }

  }

  def createNeighborsMetrix(column: Int, row: Int, totalColumn:Int, totalRow:Int)={
    val metrixX=if(totalColumn>=3) 3 else totalColumn
    val metrixY=if(totalRow>=3) 3 else totalRow

    val metrix=new Array[Array[(String,Int,Int,Boolean)]](metrixY)

    (0 until metrixY).foreach(i=>{
      val t=new Array[(String,Int,Int,Boolean)](metrixX)
      (0 until t.size).foreach(j=>{
        t(j)=null
      })
      metrix(i)=t
    })

    val x=if(column==0 ) 0 else if(column==totalColumn-1) metrixX-1 else 1
    val y=if(row==0 ) 0 else if(row==totalRow-1) metrixY-1 else 1

    metrix(y)(x)=(""+xChars(column)+yChars(row),column,row,true)
    fillMetrix(metrix,x,y,metrixX,metrixY)


    metrix
  }

  def createNeighbors(width: Int, height: Int, column: Int, row: Int,totalColumn:Int,totalRow:Int) = {
    val neighbors=creatBlankImage(width,height,Color.WHITE)
    val g=neighbors.getGraphics


    val metrix=createNeighborsMetrix(column,row,totalColumn,totalRow)
    val pad=2
    val metrixX=metrix(0).size
    val metrixY=metrix.size
    val maxX=width-(metrixX-1)*pad
    val maxY=height-(metrixY-1)*pad
    val maxUnitX=maxX/metrix(0).size
    val maxUnitY=maxY/metrix.size
    val (tImage,font)=fillString(maxUnitX,maxUnitY,0,metrix(0)(0)._1,Color.BLACK,Color.WHITE,new Font(defaultFontName,Font.PLAIN,maxUnitY))


    val wholeMetrixWidth=tImage.getWidth*metrixX
    val wholeMetrixHeight=tImage.getHeight*metrixY
    val startx=(maxX-wholeMetrixWidth)/2
    val starty=(maxY-wholeMetrixHeight)/2
    var addY=0;
    (0 until metrixY).foreach(y=>{
      var addX=0
      (0 until metrixX).foreach(x=>{
        val bkColor=if(metrix(y)(x)._4)Color.GRAY else Color.WHITE
        val (image,tf)=fillString(maxUnitX,maxUnitY,0,metrix(y)(x)._1,Color.BLACK,bkColor,font)
        g.drawImage(image,startx+addX,starty+addY,null)
        addX+=pad
        addX+=image.getWidth
      })
      addY+=pad+tImage.getHeight
    })

    neighbors
  }

  def createPageString(width: Int, height: Int, name: String) = {
    val page=creatBlankImage(width,height,Color.WHITE)
    val g=page.getGraphics
    val (pageStringImage,f)=fillString(width*3/4,height*3/4,0,name,Color.BLACK,Color.WHITE,new Font(defaultFontName,Font.PLAIN,8))

    g.drawImage(pageStringImage,width/4,height/4,null)
//    ImageIO.write(page,"png",new File("/tmp/tt/page.png"))
//    ImageIO.write(pageStringImage,"png",new File("/tmp/tt/pageString.png"))
    page
  }

  def createFooder(width: Int, height: Int, column: Int, row: Int,totalColumn:Int,totalRow:Int, name: String):BufferedImage = {

    val footer=creatBlankImage(width,height,Color.WHITE)
    val g=footer.getGraphics


    val readme=createColumnRowReadme(width/2-10,height,totalColumn,totalRow);
    val neighbors=createNeighbors(width/4,height,column,row,totalColumn,totalRow)
    val pageString=createPageString(width/4,height,name)


    g.drawImage(readme,0,0,null)
    g.drawImage(neighbors,width/2,0,null)
    g.drawImage(pageString,width*3/4,0,null)

    footer
  }

  def main(args: Array[String]): Unit = {
    val p = new ParameterParser(args)

    p.add(Parameter("help").addAliasKey("-h").setRequired(false).setFollowingValueSize(0))
    p.add(Parameter("source-image-file").setFollowingValueSize(1).setRequired(true).addAliasKey("-i"))
    p.add(Parameter("output-folder").setFollowingValueSize(1).setRequired(true).addAliasKey("-o"))

    //A4 8.27 × 11.69 , dpi 300,
    p.add(Parameter("new-image-width").setFollowingValueSize(1).setRequired(false).setDefaultValue(""+math.rint(8.27*300).toInt).setDesc("A4 8.27 × 11.69 , dpi 300"))
    p.add(Parameter("new-image-height").setFollowingValueSize(1).setRequired(false).setDefaultValue(""+math.rint(11.69*300).toInt).setDesc("A4 8.27 × 11.69 , dpi 300"))
    p.add(Parameter("image-pad").setFollowingValueSize(1).setRequired(false).setDefaultValue("100"))
    p.add(Parameter("image-pad-top").setFollowingValueSize(1).setRequired(false))
    p.add(Parameter("image-pad-left").setFollowingValueSize(1).setRequired(false))
    p.add(Parameter("image-pad-right").setFollowingValueSize(1).setRequired(false))
    p.add(Parameter("image-pad-bottom").setFollowingValueSize(1).setRequired(false))
    p.add(Parameter("footer-top-margin").setFollowingValueSize(1).setRequired(false).setDefaultValue("20"))
    p.add(Parameter("footer-height").setFollowingValueSize(1).setRequired(false).setDefaultValue("500"))
    p.add(Parameter("over-size").setFollowingValueSize(1).setRequired(false).setDefaultValue("50"))
    p.add(Parameter("font-name").setFollowingValueSize(1).setRequired(false).setDefaultValue(defaultFontName))
    p.add(Parameter("test").setFollowingValueSize(0).setRequired(false))



    p.parse()

    if (p.isSet("help")) {
      println(s"Usage:\n${p.usage}")
      return
    }
    if (p.isError()) {
      p.errorMessages.foreach(println(_))
      println(s"Usage:\n${p.usage}")
      return
    }

    val testMode=p.isSet("test")

    defaultFontName=p.getFirstValue("font-name").get

    val outputFolder=p.getFirstValue("output-folder").get
    val od=new File(outputFolder)
    if(!od.isDirectory){
      od.mkdirs()
    }
    if(!od.isDirectory){
      throw new IllegalArgumentException(s"Could not create dir:${outputFolder}")
    }

    val newImageWidth=p.getFirstValue("new-image-width").get.toInt
    val newImageHeight=p.getFirstValue("new-image-height").get.toInt
    val pad=p.getFirstValue("image-pad").get.toInt

    val footerHeight=p.getFirstValue("footer-height").get.toInt
    val footerTopMargin=p.getFirstValue("footer-top-margin").get.toInt

    val padTop=if(p.isSet("image-pad-top")) p.getFirstValue("image-pad-top").get.toInt else pad
    val padLeft=if(p.isSet("image-pad-left")) p.getFirstValue("image-pad-left").get.toInt else pad
    val padRight=if(p.isSet("image-pad-right")) p.getFirstValue("image-pad-right").get.toInt else pad
    val padBottom=if(p.isSet("image-pad-bottom")) p.getFirstValue("image-pad-bottom").get.toInt else pad


    val maxWidth=newImageWidth-padLeft-padRight
    val maxHeight=newImageHeight - padTop -padBottom-footerHeight-footerTopMargin

    val image=loadImage(p.getFirstValue("source-image-file").get)

    val imgWidth=image.getWidth
    val imgHeight=image.getHeight

    val over=p.getFirstValue("over-size").get.toInt
    var (xSize,xWidth)=getBestSliceSize(imgWidth,maxWidth,over)
    var (ySize,yHeight)=getBestSliceSize(imgHeight,maxHeight,over)


    if(xSize>xChars.length){
      throw new IllegalArgumentException(s"slice size ${xSize} is more than ${xChars.length}")
    }

    if(ySize>yChars.length){
      throw new IllegalArgumentException(s"slice size ${ySize} is more than ${yChars.length}")
    }

    val xSlice=slice(imgWidth,xSize,over)
    val ySlice=slice(imgHeight,ySize,over)

    println(s"output image width:${newImageWidth} max image width:${maxWidth} real image width:${xWidth}")
    println(s"output image height:${newImageHeight} max image height:${maxHeight} real image height:${yHeight}")
    println(s"pad left:${padLeft},top :${padTop}, right:${padRight}, bottom:${padBottom}")
    println(s"foot margin top:${footerTopMargin}, footer height:${footerHeight}")
    println(s"the number of x slice:${xSize}")
    println(s"the number of y slice:${ySize}")
    println(xSlice.mkString(","))
    println(ySlice.mkString(","))

    //debug
    //ySize=1
    //xSize=1

    // end debug

    var count=0;
    (0 until ySize).foreach(y=>{
      (0 until xSize).foreach(x=>{

        if(!testMode || count<1) {

          val wholeImage = creatBlankImage(newImageWidth, newImageHeight)
          val mg = wholeImage.getGraphics.asInstanceOf[Graphics2D]
          val page = xChars(x) + yChars(y)
          val fileName = LocalFileSystem.buildFilePath(outputFolder, page + ".png")
          println(s"working on page:${page}, x:(${xSlice(x)._1},${xSlice(x)._2})  y:(${ySlice(y)._1},${ySlice(y)._2})")
          val newImage = crop(image, xSlice(x)._1, xSlice(x)._2, ySlice(y)._1, ySlice(y)._2)

          //copy image to output
          val imageStartx = padLeft + (maxWidth - xWidth) / 2
          val imageStarty = padTop + (maxHeight - yHeight) / 2
          println(s"image start x:${imageStartx}, image start y:${imageStarty}")
          mg.drawImage(newImage, imageStartx, imageStarty, null)

          if (footerHeight > 0) {
            // draw line between footer and image
            mg.setColor(Color.BLACK)
            mg.setStroke(new BasicStroke(3))
            mg.drawLine(padLeft, padTop + maxHeight + footerTopMargin / 2, padLeft + maxWidth, padTop + maxHeight + footerTopMargin / 2)
            val footImage = createFooder(maxWidth, footerHeight, x, y, xSize, ySize, page)
            mg.drawImage(footImage, padLeft, padTop + maxHeight + footerTopMargin, null)
          }


          println(s"will write ${page} to ${fileName}")
          ImageIO.write(wholeImage, "png", new File(fileName))
        }
        count+=1

      })
    })



  }

  def loadImage(f: String) = {
    ImageIO.read(new FileInputStream(f));

  }

  def crop(image: BufferedImage, startX: Int, width: Int, startY: Int, height: Int): BufferedImage = {
    Scalr.crop(image, startX, startY, width, height)
  }

  def pad(image: BufferedImage, left: Int, top: Int, right: Int, bottom: Int, padColor: Color = Color.WHITE): BufferedImage = {
    if (left < 0) throw new IllegalArgumentException(s"pad left :${left} should not less than 0")
    if (top < 0) throw new IllegalArgumentException(s"pad top :${top} should not less than 0")
    if (right < 0) throw new IllegalArgumentException(s"right left :${right} should not less than 0")

    if (bottom < 0) throw new IllegalArgumentException(s"pad left :${bottom} should not less than 0")
    if (image == null) throw new IllegalArgumentException(s"image is null ")

    if (left == 0 && top == 0 && right == 0 && bottom == 0) {
      return image
    }

    val newWidth = image.getWidth + left + right
    val newHeight = image.getHeight() + top + bottom
    val colorHasAlpha = padColor.getAlpha != 255
    val imageHasAlpha = image.getTransparency != Transparency.OPAQUE

    val re = if (colorHasAlpha || imageHasAlpha) {

      new BufferedImage(newWidth, newHeight, BufferedImage.TYPE_INT_ARGB)
    }
    else {

      new BufferedImage(newWidth, newHeight, BufferedImage.TYPE_INT_RGB)
    }
    val g = re.getGraphics
    g.setColor(padColor)
    g.fillRect(0, 0, newWidth, newHeight)
    // Draw the image into the center of the new padded image.
    g.drawImage(image, left, top, null)
    g.dispose()
    re
  }

  def writeWord(image: BufferedImage, word: String, x: Int, y: Int, font: Font, color: Color = Color.BLACK) = {
    val g = image.getGraphics

    g.setFont(font)
    g.setColor(color)

    g.drawString(word, x, y)
  }

  def creatBlankImage(width: Int, height: Int, bgColor: Color = Color.WHITE): BufferedImage = {
    val re = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g = re.getGraphics
    g.setColor(bgColor)
    g.fillRect(0, 0, width, height)
    re
  }

  def slice(total: Int, sliceNum: Int, overSize: Int): Array[(Int, Int)] = {

    if (sliceNum < 1 || sliceNum > total / 2) {
      throw new IllegalArgumentException(s"sliceSize should be more than 0 and less ${total / 2}")
    }

    if (total / sliceNum / 2 < overSize) {
      throw new IllegalArgumentException(s"overSize should be more less  ${total / sliceNum / 2}")
    }

    val ret = new Array[(Double, Double)](sliceNum)

    val eachSize = (total + (sliceNum - 1) * overSize).toDouble / sliceNum


    ret(0) = (0, eachSize)

    (1 until sliceNum).foreach(i => {
      //println(s"${i}  last:${ret(i - 1)._2 + ret(i - 1)._1}")
      ret(i) = (ret(i - 1)._2 + ret(i - 1)._1 - overSize, eachSize)
    })

    //ret.foreach(a => println(s"${a} ${a._1 + a._2}"))
    ret.map(a => (Math.rint(a._1).toInt, Math.rint(a._2).toInt))
  }

  def getStringSize(text: String, font: Font): (Int, Int) = {


    val affinetransform = new AffineTransform();
    val frc = new FontRenderContext(affinetransform, true, true);


    val textwidth = (font.getStringBounds(text, frc).getWidth());
    val textheight = (font.getStringBounds(text, frc).getHeight());

    (Math.rint(textwidth).toInt, Math.rint(textheight).toInt)
  }
  def getStringSize2(text: String, font: Font,g:Graphics2D): (Int, Int) = {

    val fm=g.getFontMetrics(font)

    (fm.stringWidth(text), fm.getHeight)
  }

  def tryIncreaseFont(maxw: Int, maxh: Int, text: String, currentFont: Font,g:Graphics2D) = {
    var found=false
    var  font=currentFont
    while(!found){
      val newFont=new Font(font.getFontName(),Font.PLAIN,font.getSize+1)
      val (w,h)=getStringSize2(text,newFont,g)
      //println(s"try increase: w:${w} h:${h} , max w:${maxw} max h:${maxh}")
      if(w<=maxw && h <= maxh ){
        font=newFont
      }
      else{
        found=true
      }
    }
    //println(s"find fond size:${font.getSize}")
    font
  }
  def tryDecreaseFont(maxw: Int, maxh: Int, text: String, currentFont: Font,g:Graphics2D) = {
    var found=false
    var  font=currentFont
    while(!found && font.getSize>=1){
      val newFont=new Font(font.getFontName(),Font.PLAIN,font.getSize-1)
      val (w,h)=getStringSize2(text,newFont,g)
      if(w<=maxw && h <= maxh ){

        found=true
      }
      font=newFont


    }
    font
  }

  def fillString(width:Int, height:Int, pad:Int, text:String, stringColor:Color, bkColor:Color, font:Font):(BufferedImage,Font)={
    val maxw=width-pad*2
    val maxh=height-pad*2
    val re=creatBlankImage(width,height,bkColor)
    val g=re.getGraphics.asInstanceOf[Graphics2D]
    var f=font

    val (fw,fh)=getStringSize2(text,f,g)

    if(fw<=maxw && fh<=maxh){
      //try to increase the font size
      f=tryIncreaseFont(maxw,maxh,text,f,g)
    }
    else {
      f=tryDecreaseFont(maxw,maxh,text,f,g)
    }

    val (w,h)=getStringSize2(text,f,g)
    g.setFont(f)
    g.setColor(stringColor)
    g.drawString(text,pad,pad+h)
    (re,f)
  }
}
