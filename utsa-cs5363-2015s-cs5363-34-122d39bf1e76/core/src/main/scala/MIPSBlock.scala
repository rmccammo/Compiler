package edu.utsa.cs5363

import java.util.ArrayList

//NOTE: I'm pretty sure I don't need this class; I'm keeping it anyway just to be safe
class MIPSBlock {
  var instructions = new ArrayList[MIPSInstruction]
  
  def addInstr(newInstr:MIPSInstruction){
    instructions.add(newInstr)
  }
  
  def getInstr(num:Int):MIPSInstruction = instructions.get(num)
  
  def size():Int = instructions.size()
}