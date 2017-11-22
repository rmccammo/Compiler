package edu.utsa.cs5363


import scala.io.Source
import java.util.Arrays

case class SyntaxError(msg: String) extends Exception(msg)

object CoreCompiler {
  def main(args: Array[String]) {

    if (args.length == 0) {
      Console.err.println("No arguments.")
      sys.exit(0)
    }

    for (arg <- args 
         if !arg.endsWith(".tl")) {
      Console.err.println("Input file name, $arg, doesn't end with \".tl\"")
      sys.exit(1)
    }

    for (arg <- args;
         fileNameStem = arg.replaceFirst(".tl$","")) {
      try {
        val sourceName = fileNameStem + ".tl"
        val parseTreeName = fileNameStem + ".pt.dot"
        val astName = fileNameStem + ".ast.dot"
        val ilocCFGName = fileNameStem + ".A3.cfg.dot"
        val mipsAsmName = fileNameStem + ".s"
        
        val source = Source.fromFile(sourceName)
        val p = new Parser(sourceName, parseTreeName, astName, ilocCFGName, mipsAsmName)
        
        
        //Important code: comment this only for testing/////////////////////////////////////// 
        p.parseFile()
        ///////////////////////////////////////////////////////////////////////
        
        // Replace this with your with calls to your parser, etc.
        /*Console.println(s"\n *** $fileNameStem")
        for (line <- source.getLines)
          Console.println(line)*/
          

      } catch {
        case e: SyntaxError => {
          print(s"Syntax Error [$fileNameStem.tl]: ")
          println(e.getMessage())
        }
        case e: Exception => {
          print(s"Error processing [$fileNameStem.tl]: ")
          println(e.getMessage())
          e.printStackTrace()
        }
      }
    }
  }
}

