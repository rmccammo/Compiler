package edu.utsa.cs5363

import java.util.ArrayList

//This class describes a node of an AST
class ASTNode(name:String) {
  var subTree = new ArrayList[ASTNode] //This list contains all children of this node; if no children, this is empty
  var nodeType = "NA" //Type (int or bool) of the node; "NA" by default
  var typeOK = true //Whether or not the typing of the subtree headed by this node is correct
  
  //return the name of the node
  def name():String = name
  
  
  //returns the size of the subTree
  def subTreeSize() = subTree.size
  
  //returns the specified child of the current node
  def getNode(index:Int):ASTNode = {
    subTree.get(index)
  }
  
  //Returns the type of this node
  def getNodeType():String = nodeType
  
  //Sets the node's type
  def setNodeType(newType:String) {nodeType = newType}
  
  //Return the typeOK of this node
  def getTypeOK():Boolean = typeOK
  
  //Sets the node's typeOK
  def setTypeOK(newTypeOK:Boolean) {typeOK = newTypeOK}
  
  //adds the specified node to the subtree
  def addNode(newNode:ASTNode){
    subTree.add(newNode)
  }
  
  //prints the contents of the subtree headed by the current node (at the head of a tree, levelNum = 0)
  def print(levelNum:Int){
    //For each level down, add some spaces
    
      if(levelNum != 0)
        for(i <- 0 to levelNum - 1)
          Console.print("  ")
    
      Console.println(name) //prints the name of this node
      
      if(subTree.size != 0)
        for(i <- 0 to subTree.size - 1)
          getNode(i).print(levelNum + 1)
      
          
      if(levelNum == 0) Console.println("")    
  }
}