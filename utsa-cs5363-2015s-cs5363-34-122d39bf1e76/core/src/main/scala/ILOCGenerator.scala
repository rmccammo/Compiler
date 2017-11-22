package edu.utsa.cs5363

import java.util.ArrayList
import java.io.File

class ILOCGenerator(fileName:String, tree:AST, varList:ArrayList[Variable]) {
  var currentReg = 0 //The number to the next register to be added
  var currentBlock = 0
  var instructionBlocks = new ArrayList[InstructionBlock]
  instructionBlocks.add(new InstructionBlock())
  
  //Code for the MIPS generator
  var reg:Map[String, Int] = Map() //maps names of ILOC registers to corresponding MIPS offsets
  var offset = 4
  
  //Invoke this when using a new register; returns the number of that register 
  def newReg():Int = {
    currentReg = currentReg + 1
    
    //map the new register to a new offset
    reg += ("r" + (currentReg - 1) -> getOffset())
    
    currentReg - 1
  }
  
  //Invoke this when using a new block; returns the number of that blcok 
  def newBlock():Int = {
    currentBlock = currentBlock + 1
    
    instructionBlocks.add(new InstructionBlock())
    currentBlock
  }
  
  //Adds a new instruction to the end of the current block
  def newInstr(name:String){
    val instr = new Instruction(name)
    instructionBlocks.get(instructionBlocks.size - 1).add(instr)
  }
  
  def newInstr(instr:Instruction){
    instructionBlocks.get(instructionBlocks.size - 1).add(instr)
  }
  
  //Adds an instruction to a specific block (use this for whiles and ifs)
  def newSpecInstr(name:String, blockNum:Int){
    val instr = new Instruction(name)
    instructionBlocks.get(blockNum).add(instr)
  }
  
  def newSpecInstr(instr:Instruction, blockNum:Int){
    instructionBlocks.get(blockNum).add(instr)
  }
  
  //returns the specified block
  def getBlock(num:Int):InstructionBlock = instructionBlocks.get(num)
  
  //return the block at the end of the list
  def lastBlock():InstructionBlock = instructionBlocks.get(instructionBlocks.size - 1)
  
  //returns the list of blocks
  def getBlockList():ArrayList[InstructionBlock] = instructionBlocks
  
  //returns a unique offset value; subtracts 4 from the current value and returns it
  def getOffset():Int = {
    offset = offset - 4
    
    offset
  }
  
  //return the map
  def getMap():Map[String, Int] = reg
  
  //Generators///////////////////////////////////////////////////////////////////////////////////
  
  //Main method: uses the AST to generate a sequence of ILOC instructions
  def genProgram(){
    val head = tree.getHead()
    
    genDeclList(head.getNode(0))
    genStmtList(head.getNode(1))
    
    val instr = new Instruction("exit")
    instr.setType("exit")
    newInstr(instr)
    
    fillEmptyBlocks()
  }
  
  def genDeclList(node:ASTNode){
    for(i <- 0 to node.subTreeSize() - 1)
      genDecl(node.getNode(i))
  }
  
  def genDecl(node:ASTNode){
    val declType = node.getNode(0).name()
    val varName = node.name().replace("ident: ", "")
    
    if(declType.equals("int")){
      var intInstr = new Instruction("loadI 0 => r_" + varName)
      intInstr.setType("loadI")
      intInstr.setIn1("0")
      intInstr.setOut1("r_" + varName)
      newInstr(intInstr)
      
      //Map the new variable register to a new offset
      reg += ("r_" + varName -> getOffset())
    }
    else{ //if type is a boolean
      var boolInstr = new Instruction("loadI false => r_" + varName)
      boolInstr.setType("loadI")
      boolInstr.setIn1("false")
      boolInstr.setOut1("r_" + varName)
      newInstr(boolInstr)
      
      //Map the new variable register to a new offset
      reg += ("r_" + varName -> getOffset())
    }
  }
  
  def genStmtList(node:ASTNode){
    for(i <- 0 to node.subTreeSize() - 1)
      genStmt(node.getNode(i))
  }
  
  def genStmt(node:ASTNode){
    val name = node.name()
    
    if(name.equals(":=")) genAssign(node)
    else if(name.equals(":= readInt")) genReadInt(node)
    else if(name.equals("while")) genWhile(node)
    else if(name.equals("if")) genIf(node)
    else if(name.equals("writeInt")) genWriteInt(node)
  }
  
  
  def genAssign(node:ASTNode):String = {
    var reg = ""
    
    val assignReg = genExpr(node.getNode(1)) //the register of the value being assigned
    val varReg = "r_" + node.getNode(0).name() //the name of the variable we're writing to
    
    //Storage instruction (e.g. i2i r3 => r_X)
    var instr = new Instruction("i2i " + assignReg + " => " + varReg)
    instr.setType("i2i")
    instr.setIn1(assignReg)
    instr.setOut1(varReg)
    newInstr(instr)
    
    reg
  }
  
  def genReadInt(node:ASTNode):String = {
    var reg = ""
    
    val varReg = "r_" + node.getNode(0).name()
    
    var instr = new Instruction("readInt " + varReg)
    instr.setType("readInt")
    instr.setIn1(varReg)
    newInstr(instr)
    
    reg
  }
  
  def genWhile(node:ASTNode):String = {
    var reg = ""
    
    //add an instruction to jump to the next block
    var jumpInstr = new Instruction("jumpI b" + instructionBlocks.size)
    jumpInstr.setType("jump")
    jumpInstr.setIn1("b" + instructionBlocks.size)
    newInstr(jumpInstr)
    
    //create a new block to hold the condition
    lastBlock().setChild1(instructionBlocks.size)
    val conditionNum = newBlock() //the number of the block in which we put the condition
    
    //evaluate the while condition (the first child of the head node
    val conditionReg = genExpr(node.getNode(0))
    
    //make a new block for the loop body and evaluate it
    instructionBlocks.get(conditionNum).setChild1(instructionBlocks.size) //The first child of the condition block is the next block
    val bodyNum = newBlock()
    genStmtList(node.getNode(1)) //evaluate the statements of the loop body
    
    //Add an instruction to the end of the loop body to jump back to the condition
    var instr = new Instruction("jumpI b" + conditionNum)
    instr.setType("jump")
    instr.setIn1("b" + conditionNum)
    newInstr(instr)
    
    //Set the child of the loop body to be the condition block
    lastBlock().setChild1(conditionNum)
    
    //add a branch instruction to the condition block
    var instr2 = new Instruction("cbr " + conditionReg + " -> b" + bodyNum + ", b" + instructionBlocks.size)
    instr2.setType("cbr")
    instr2.setIn1(conditionReg)
    instr2.setOut1("b" + bodyNum)
    instr2.setOut2("b" + instructionBlocks.size)
    newSpecInstr(instr2, conditionNum)
    
    //add a new block to go to once the loop has terminated
    instructionBlocks.get(conditionNum).setChild2(instructionBlocks.size)
    newBlock()
    
    
    reg
  }
  
  def genIf(node:ASTNode):String = {
    var reg = ""
    
    //create a new block to hold the condition
    //lastBlock().setChild1(instructionBlocks.size)
    val conditionNum = instructionBlocks.size() - 1 //the number of the block in which we put the condition
    
    //evaluate while condition
    val conditionReg = genExpr(node.getNode(0))
    
    //Make a block for if the if condition is true and evaluate it
    instructionBlocks.get(conditionNum).setChild1(instructionBlocks.size) //The first child of the condition block is the next block
    val ifNum = newBlock() //number of the block of code to be executed if the condition is true
    genStmtList(node.getNode(1))
    
    //make a new block to go to if the condition is false (we include this block even without an else)
    instructionBlocks.get(conditionNum).setChild2(instructionBlocks.size) //The first child of the condition block is the next block
    val elseNum = newBlock()
    
    //Now that we know where the condition block's children are, we add a branch statement to that block
    var branchInstr = new Instruction("cbr " + conditionReg + " -> b" + ifNum + ", b" + elseNum)
    branchInstr.setType("cbr")
    branchInstr.setIn1(conditionReg)
    branchInstr.setOut1("b" + ifNum)
    branchInstr.setOut2("b" + elseNum)
    newSpecInstr(branchInstr, conditionNum)
    
    //evaluate else condition, if it exists (node will have 3 children)
    if(node.subTreeSize == 3){
      genStmtList(node.getNode(2).getNode(0)) //3rd node is else, with a stmt list node as a child
    }
    
    
    //Add a exit block, which the if and else blocks link to
    val exitNum = newBlock()
    instructionBlocks.get(ifNum).setChild1(exitNum)
    instructionBlocks.get(elseNum).setChild2(exitNum)
    
    //add an instruction to jump to the exit block, add it to the if and else blocks
    var exitInstr = new Instruction("jumpI b" + exitNum)
    exitInstr.setType("jump")
    exitInstr.setIn1("b" + exitNum)
    
    newSpecInstr(exitInstr, ifNum)
    newSpecInstr(exitInstr, elseNum)
    
    reg
  }
  
  def genWriteInt(node:ASTNode):String = {
    var reg = ""
    
    val writeReg = genExpr(node.getNode(0))
    
    var instr = new Instruction("writeInt " + writeReg)
    instr.setType("writeInt")
    instr.setIn1(writeReg)
    newInstr(instr)
    
    reg
  }
  
  //Generates instructions for an expression, returns the register the solution is stored at
  def genExpr(node:ASTNode):String = {
    var reg = ""
    val name = node.name()
    
    //get expression type
    val exprType = name match {
      case "+" => "add"
      case "-" => "sub"
      case "*" => "mult"
      case "div" => "div"
      case "mod" => "mod"
      case "<" => "cmp_LT"
      case "<=" => "cmp_LTE"
      case ">" => "cmp_GT"
      case ">=" => "cmp_GTE"
      case "=" => "cmp_EQ"
      case "!=" => "cmp_NE"
      case _ => "factor"
    }
    
    
    if(exprType.equals("factor")){
      reg = genFactor(node)
    }
    else{
      //get the registers of the two operands of the expression (the two children of the expression node)
      val reg1 = genExpr(node.getNode(0))
      val reg2 = genExpr(node.getNode(1))
      val outReg = ("r" + newReg())
      
      //creates a new instruction to represent the expression (e.g. add r1, r2 => r0)
      var instr = new Instruction(exprType + " " + reg1 + ", " + reg2 + " => " + outReg)
      instr.setType(exprType)
      instr.setIn1(reg1)
      instr.setIn2(reg2)
      instr.setOut1(outReg)
      
      newInstr(instr)
      reg = outReg
    }
    
    
    reg
  }
  
  //Generates instructions for a factor (e.g. 2, X) if appropriate, and returns a String representing the register
  //the factor is stored at
  def genFactor(node:ASTNode):String = {
    var reg = "" //return value
    val name = node.name() //name of the node
    
    //if the factor is an int or bool, invoke a loadI and store the value in a new register
    if(name.matches("[1-9][0-9]*|0") || name.equals("true") || name.equals("false")){
      val regNum = ("r" + newReg()) //get the number of a new register
      
      var instr = new Instruction("loadI " + name + " => " + regNum) //loadI c => r0
      instr.setType("loadI")
      instr.setIn1(name)
      instr.setOut1(regNum)
      
      newInstr(instr)
      reg = regNum
    }
    //otherwise, the factor is a variable; no new instruction is needed, we just return the register relating to that variable
    else{
      reg = "r_" + name
    }
    
    
    reg
  }
  
  //This program may contain empty blocks (e.g. when any two if/whiles are executed in succession). This statement adds a jump
  //instruction to the next block for these empty blocks
  def fillEmptyBlocks(){
    for(i <- 0 to instructionBlocks.size - 1){
      if(instructionBlocks.get(i).size() == 0){
        instructionBlocks.get(i).setChild1(i + 1) //sets the child of the block
        
        //adds a jump instruction
        var jumpInstr = new Instruction("jump b" + (i + 1))
        jumpInstr.setType("jump")
        jumpInstr.setIn1("b" + (i + 1))
        
        newSpecInstr(jumpInstr, i)
      }
    }
  }
  
  def writeToFile(){
    val writer = new java.io.PrintWriter(new File(fileName))
    
    writer.println("digraph graphviz {")
    writer.println("node [shape = none];")
    writer.println("edge [tailport = s];")
    writer.println("entry")
    
    //Tables
    writer.println("subgraph cluster {")
    
    writer.println("color = \"/x11/white\"")
    //Print the contents of the blocks
    for(i <- 0 to instructionBlocks.size - 1)
      writeBlock(writer, instructionBlocks.get(i), i)
    
    writer.println("}")
    //end of tables
    
    writer.println("entry -> n0")
    writer.println("n" + (instructionBlocks.size() - 1) + " -> exit")
    writer.println("}")
    
    writer.close()
  }


  def writeBlock(writer:java.io.PrintWriter, newBlock:InstructionBlock, blockNum:Int) {
    writer.print("n" + blockNum + " [label=<<table border=\"0\"><tr><td border=\"1\" colspan=\"3\">B" + blockNum + "</td></tr>")
    
    //print the instructions
    for(i <- 0 to newBlock.size() - 1){
      var currentInstr = newBlock.getInst(i).getName()
      currentInstr = currentInstr.replace(">", "&gt;") //Replaces brackets in "=>" with proper character, so it can be represented
      writer.print("<tr><td>" + currentInstr + "</td></tr>")
    }
    
    writer.print("</table>>,fillcolor=\"/x11/white\",shape=box]")
    writer.println("") //ends the current line
    
    //print connections to block's child(ren), if defined
    if(newBlock.getChild1() != -1) writer.println("n" + blockNum + " -> n" + newBlock.getChild1())
    if(newBlock.getChild2() != -1) writer.println("n" + blockNum + " -> n" + newBlock.getChild2())
  }

}