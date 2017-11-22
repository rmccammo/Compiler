package edu.utsa.cs5363

//Stores a possible node in the parse tree
class Node(text:String, parent:Int) {
  def getText():String = text
  def getParent():Int = parent
}