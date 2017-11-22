package edu.utsa.cs5363

class Instruction(init:String) {
  var name = init //Ex: add r1, r2 => r3
  //var splitName = name.split("""\s""") //splits the instruction into tokens, so we can evaluate its individual parts
  var instrType = "" //type of the instruction (add, sub, etc.)
  var in1 = "" //instructions have 1-2 input regs/blocks, and 1-2 output regs-blocks; these variables record these
  var in2 = ""
  var out1 = ""
  var out2 = ""
  
  //setInstruction()
  
  
  def getName():String = name
  
  
  
  //sets the values in the instruction according to type (do this for MIPS)
  /*def setInstruction(){
    instrType = splitName(0)
    
    
  }*/
  
  //Getters for instruction operands
  def getType():String = instrType
  
  def getIn1():String = in1
  
  def getIn2():String = in2
  
  def getOut1():String = out1
  
  def getOut2():String = out2
  
  //Setters for operands
  def setType(newType:String) {instrType = newType}
  
  def setIn1(newIn:String) {in1 = newIn}
  
  def setIn2(newIn:String) {in2 = newIn}
  
  def setOut1(newOut:String) {out1 = newOut}
  
  def setOut2(newOut:String) {out2 = newOut}
}