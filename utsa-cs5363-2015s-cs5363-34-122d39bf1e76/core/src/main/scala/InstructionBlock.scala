package edu.utsa.cs5363

import java.util.ArrayList

class InstructionBlock {
  var InstructionList = new ArrayList[Instruction]
  
  var child1 = -1 //Indices of the block(s) stemming from this block
  var child2 = -1
  
  def add(newInstr:Instruction){
    InstructionList.add(newInstr)
  }
  
  def getInst(num:Int):Instruction = InstructionList.get(num)
  
  def size():Int = InstructionList.size()
  
  def getChild1():Int = child1
  
  def getChild2():Int = child2
  
  def setChild1(num:Int) {child1 = num}
  
  def setChild2(num:Int) {child2 = num}
  
  def setChildren(num1:Int, num2:Int){
    child1 = num1
    child2 = num2
  }
}