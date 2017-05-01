import java.io.FileInputStream
import javax.imageio.ImageIO

import com.aol.advertising.ml.plib.commandline_parameters.{Parameter, ParameterParser}

/**
  * Created by mtao60 on 4/29/17.
  */
object ShowImageInfo {

  def main(args: Array[String]): Unit = {
    val p = new ParameterParser(args)

    p.add(Parameter("help").addAliasKey("-h").setRequired(false).setFollowingValueSize(0))
    p.add(Parameter("source-image-file").setFollowingValueSize(1).setRequired(true).addAliasKey("-i"))
        p.enableStandaloneParameters
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

    val  buff = ImageIO.read(new FileInputStream(p.getFirstValue("source-image-file").get));

    println(s"width:${buff.getWidth} , height:${buff.getHeight}")

  }
}
