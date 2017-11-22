package edu.utsa.cs5363

import java.io.File
import java.util.ArrayList

//Contains an abstract syntax tree headed by an ASTNode, which contains all its children; fileName is the name of the file to write the AST to
class AST(head:ASTNode, fileName:String) {
  var currentNum = 0 //the number of the current node we are looking at when writing to a file
  var variables =  new ArrayList[Variable] //holds all the variables in the ast
  
  //prints the contents of the tree
  def print(){ 
    head.print(0)
    }
  
  def getHead():ASTNode = head
  
  //checks if the typing of the AST is valid
  def typeCheck():Boolean = {
    getVariables()
    
    val check = stmtListType(head.getNode(1))
    Console.println("Types valid for " + fileName + ": " + check)
    check
  }
  
  def vars():ArrayList[Variable] = variables
  
  //gets the variables from the declarations side of the tree
  def getVariables(){
    variables = new ArrayList[Variable] //Resets the contents of the variable list
    val decl = head.getNode(0) //Gets the declarations list
    var varName = ""
    var varType = ""
    
    //If the node marks the beginning of a declaration list, use the subtree to fill the variable list
    if(decl.name().equals("decl list")){
      for(i <- 0 to decl.subTreeSize() - 1){
        varName = decl.getNode(i).name()
        varName = varName.replaceFirst("ident: ", "") //Remove the "ident: " at the start of a node name, leaving just the variable name
        varType = decl.getNode(i).getNode(0).name()
        
        variables.add(new Variable(varName, varType))
      }
    }
  }
  
  //checks if the given variable name exists in the variable list; returns the index of the variable or -1 if not found
  def isVar(varName:String):Int = {
    var varIndex = -1
    
    for(i <- 0 to variables.size - 1){
      if(variables.get(i).getName().equals(varName) && varIndex == -1) varIndex = i
    }
    
    varIndex
  }
  
  //gets the type of a var
  def varType(subTree:ASTNode):String = {
    var vType = "error"
    val varIndex = isVar(subTree.name())
    
    if(varIndex != -1) vType = variables.get(varIndex).getType()
    
    vType
  }
  
  //checks the types of a list of statements
  def stmtListType(subTree:ASTNode):Boolean = {
    var sType = true
    
    //iterates through all statements, checks their types
    for(i <- 0 to subTree.subTreeSize() - 1){
      if(!stmtType(subTree.getNode(i))) sType = false //
    }
    
    subTree.setTypeOK(sType)
    sType
  }
  
  //checks the type of a statement
  def stmtType(subTree:ASTNode):Boolean = {
    var sType = false
    
    //Console.println("TEST: " + subTree.name().equals(":="))
    //checks an assignment
    if(subTree.name().equals(":=") || subTree.name().equals(":= readInt")) sType = assignmentType(subTree)
    
    //checks an if
    else if(subTree.name.equals("if")) sType = ifType(subTree)
    
    //checks a while
    else if(subTree.name.equals("while")) sType = whileType(subTree)
    
    //checks a writeInt
    else if(subTree.name.equals("writeInt")) sType = writeIntType(subTree)
    
    subTree.setTypeOK(sType)
    sType
  }
  
  //checks the type of an assignment
  def assignmentType(subTree:ASTNode):Boolean = {
    var typeOK = false
    var vType = varType(subTree.getNode(0)) //get the type of the variable
    //if the assignment is to a readInt, we only check the type of one node
    if(subTree.name().equals(":= readInt")){
      if(vType.equals("int")) typeOK = true //if the value being assigned is an int
    }
    
    //if it's just an assignment, make sure the types match
    else{
      
      var eType = exprType(subTree.getNode(1)) //get the type of the Expression used to make the assignment
      
      if(vType.equals(eType) && !vType.equals("error") && !eType.equals("error")) typeOK = true
    }
    
    subTree.setTypeOK(typeOK)
    typeOK
  }
  
  //checks the type of an if
  def ifType(subTree:ASTNode):Boolean = {
    var iType = false
    
    //the type of the first subtree (the condition) must be a boolean
    val conType = exprType(subTree.getNode(0)).equals("bool")
    
    //the type of the 2nd subtree(a stmt list) must be valid
    val stmtType = stmtListType(subTree.getNode(1))
    
    //if there is an else statement, its type must be valid
    var elseType = true
    if(subTree.subTreeSize() == 3){
        //check the type of the subtree's 3rd node's (else) 1st node (stmt list) 
       elseType = stmtListType(subTree.getNode(2).getNode(0))
    }
    
    if(conType && stmtType && elseType) iType = true
    
    subTree.setTypeOK(iType)
    iType
  }
  
  //checks the type of a while
  def whileType(subTree:ASTNode):Boolean = {
    var wType = false
    
    //the type of the first subtree (the condition) must be a boolean
    val conType = exprType(subTree.getNode(0)).equals("bool")
    
    //the type of the 2nd subtree(a stmt list) must be valid
    val stmtType = stmtListType(subTree.getNode(1))
    
    if(conType && stmtType) wType = true
    
    subTree.setTypeOK(wType)
    wType
  }
  
  //checks the type of a writeInt
  def writeIntType(subTree:ASTNode):Boolean = {
    var wType = false
    
    //get the type of the expression for which we are writing the int to
    var eType = exprType(subTree.getNode(0))
    if(eType.equals("int")) wType = true
      
    
    subTree.setTypeOK(wType) 
    
    wType
  }
  
  //gets the type of an expression
  def exprType(subTree:ASTNode):String = {
    var eType = "error"
    
    //if the node is a comparative, check if the subexpressions are ints
    if(isCompare(subTree)){
      if(exprType(subTree.getNode(0)).equals("int") && simpleExprType(subTree.getNode(1)).equals("int")) eType = "bool"
    }
    //if the node is not a multiplicative, it's a factor
    else eType = simpleExprType(subTree)
      
    
    if(!eType.equals("error")) subTree.setNodeType(eType)
    else subTree.setTypeOK(false)
    eType
  }
  
  //gets the type of a simple expression
  def simpleExprType(subTree:ASTNode):String = {
    var seType = "error"
    
    //if the node is an additive, check if the subexpressions are ints
    if(isAdditive(subTree)){
      if(simpleExprType(subTree.getNode(0)).equals("int") && termType(subTree.getNode(1)).equals("int")) seType = "int"
    }
    //if the node is not a multiplicative, it's a factor
    else seType = termType(subTree)
      
    
    if(!seType.equals("error")) subTree.setNodeType(seType)
    else subTree.setTypeOK(false)
    seType
  }
  
  //gets the type of a term
  def termType(subTree:ASTNode):String = {
    var tType = "error"
    
    //if the node is a multiplicative, check if the subexpressions are ints
    if(isMultiplicative(subTree)){
      if(termType(subTree.getNode(0)).equals("int") && factorType(subTree.getNode(1)).equals("int")) tType = "int"
    }
    //if the node is not a multiplicative, it's a factor
    else tType = factorType(subTree)
    
    if(!tType.equals("error")) subTree.setNodeType(tType)
    else subTree.setTypeOK(false)
      
    tType
  }
  
  //gets the type of a factor
  def factorType(subTree:ASTNode):String = {
    var fType = "error"
    val varIndex = isVar(subTree.name())
    //checks if the factor is an int
    if(subTree.name().matches("[1-9][0-9]*|0")) {
      fType = "int"
      subTree.setNodeType(fType)
      }
    
    //check if the factor is a bool
    else if(subTree.name().matches("false|true")) {
      fType = "bool"
      subTree.setNodeType(fType)
      }
    
    
    //check if the factor is an existing variable
    else if(varIndex != -1) {
      fType = variables.get(varIndex).getType()
      subTree.setNodeType(fType)
      }
    
    //If no valid type could be retrieved, set typeOK of the subtree to false
    if(fType.equals("error")) subTree.setTypeOK(false)
    
    fType
  }
  
  
  
  //checks if the node is a comparator
  def isCompare(node:ASTNode):Boolean = {
    var isComp = false
    
    if(node.name().equals("=") || node.name().equals("!=") || node.name().equals("<") || node.name().equals(">") || node.name().equals("<=") || node.name().equals(">=")) isComp = true
    
    isComp
  }
  
  //checks if the node is an additive
  def isAdditive(node:ASTNode):Boolean = {
    var isAdd = false
    
    if(node.name().equals("+") || node.name().equals("-")) isAdd = true
    
    isAdd
  }
  
  //checks if the node is a multiplicative
  def isMultiplicative(node:ASTNode):Boolean = {
    var isMul = false
    
    if(node.name().equals("*") || node.name().equals("div") || node.name().equals("mod")) isMul = true
    
    isMul
  }
  
  //writes the contents of the tree to a file
  def writeToFile(){
    val writer = new java.io.PrintWriter(new File(fileName))
    writer.println("digraph g {")
    writer.println("node[style = filled]")
    //write the head
    writer.println("n0 [label=\"" + head.name() + "\"]")
    
    //write the contents of the subtree
    if(head.subTreeSize() != 0){
      for(i <- 0 to head.subTreeSize() - 1){
        WriteNode(writer, head.getNode(i), 0)
      }
    }
    
    writer.println("}")
    writer.close()
    currentNum = 0
  }
  
  //Writes the specified node with the specified parent number to a file
  def WriteNode(writer:java.io.PrintWriter, newNode:ASTNode, parentNum:Int){
    //since this is a new node, we increment currentNum
    currentNum = currentNum + 1
    
    //writes a new node in the graph file, and connects it to the parent
    if(!newNode.getTypeOK()) //Color the node red if type isn't OK
      writer.println("n" + currentNum + " [label=\"" + newNode.name + "\",fillcolor=\"/pastel13/1\"]")
    else
      writer.println("n" + currentNum + " [label=\"" + newNode.name + "\"]")
    writer.println("n" + parentNum + " -> n" + currentNum)
    
    //write all the nodes in the subtree
    if(newNode.subTreeSize() != 0){
      val nodeNum = currentNum //saves the index of this node for generating its children
      
      for(i <- 0 to newNode.subTreeSize() - 1){
        WriteNode(writer, newNode.getNode(i), nodeNum)
      }
    }
  }
  
}