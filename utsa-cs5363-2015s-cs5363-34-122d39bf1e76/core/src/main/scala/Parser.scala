package edu.utsa.cs5363

import scala.io.Source
import java.util.ArrayList
import java.util.Arrays
import java.io.File

class Parser(sourceName: String, fileName: String, ASTName:String, ilocCFGName:String, mipsAsmName:String){
     object ParseError extends Exception //A ParseError to be thrown when the program is incorrect
     
     val source = Source.fromFile(sourceName)
     var NodeList = new ArrayList[Node] //The parse tree: stores all the nodes
     val nodeNum = 0 //Number of the next node to be added
     
     var lines = new ArrayList[String] //All the lines from a file
     var currentLine = "" //Contains the current line being read in the file
     var splitLine = new Array[String](10) //Contains currentLine, split into separate terms
     var lineNum = 0 //The number of the next line to be read
     
     var varList = new ArrayList[Variable] //List of all variables declared in the program
     
     /*NodeList.add(new Node("program", 0))
     NodeList.add(new Node("PROGRAM", 0))
     NodeList.add(new Node("declarations", 0))*/
     
     
     //Using the source name, parses the specified file
     def parseFile(){
       //reads the contents of the source file into lines
       for(line <- source.getLines)
         lines.add(line)
        
         currentLine = lines.get(0)
         splitLine = currentLine.split("""\s""")
        parseProgram() 
         
         
         writeToFile()
     }
     
     //Start symbol: parses the program symbol; also continues translating the file once complete
     def parseProgram(){
       if(splitLine(0).equals("program")){
         NodeList.add(new Node("program", 0))
         //Create initial ASTNode
         val headNode = new ASTNode("program")
         
         parseTerminal("PROGRAM", 0)
         
         //for the declarations list
         val declNode = new ASTNode("decl list") //New ASTNode to head the list
         
         
         
         val list = parseDeclarations(NodeList.size, 0) //All the declaration subtrees, in an arrayList
         for(i <- list.size - 1 to 1 by -1)
           declNode.addNode(list.get(i))
         headNode.addNode(declNode)
         
         parseTerminal("BEGIN", 0)
         
         val statementNode = new ASTNode("stmt list")
         val statements = parseStatementSequence(NodeList.size, 0)
         for(i <- statements.size - 1 to 0 by -1){statementNode.addNode(statements.get(i))}
         headNode.addNode(statementNode)
         
         
         parseTerminal("END", 0)
         
         
         //Build and check the AST/////////////////////////////////////////////////
         val tree = new AST(headNode, ASTName)
         val check = tree.typeCheck() //checks that all typing is valid
         
         
         tree.writeToFile()
         
         //ilocCFG step (go past this point only if types are valid)///////////////////////
         if(check){
           val cfg = new ILOCGenerator(ilocCFGName, tree, tree.vars())
                 
           
           
           cfg.genProgram()        
           cfg.writeToFile() 
           
           
           //Generate MIPS code///////////////////////////////////////////////////////
           val mips = new MIPSGenerator(cfg.getBlockList(), cfg.getMap(), mipsAsmName)
           mips.genMIPS()
         }
 
         
         lineNum = lineNum + 1
       }
       else throw ParseError
     }
     
     //Add a declarations node and parses it; note that a new line is required
     //indices - 1 = var, 2 = var name, 3 = as, 4 = int or bool 5 = ;
     //For all declarations, returns a list of each ASTNode subtree NOTE: this list will be in reverse order; the first (i.e. last) node
     //will say "blank," because it represents the empty string at the end of the declarations, but we won't use that node
     def parseDeclarations(nodeNum:Int, parentNum:Int):ArrayList[ASTNode] = {
       nextLine()
       var newList = new ArrayList[ASTNode]

       NodeList.add(new Node("declarations", parentNum))
       
       //If the new line is begin, we're at the end of declarations, so parse an empty string
       if(splitLine(0).equals("begin")){
         parseTerminal("&#949;", nodeNum)
         
         newList.add(new ASTNode("blank"))
       } 
       //Parse for assignment if var is detected
       else{
         if(splitLine(0).equals("var")) parseTerminal("VAR", nodeNum)
         else throw ParseError
         
         //parse the variable name, but don't add it yet because we don't know its type yet
         //Create an ASTNode for the variable
         val declNode = parseTerminal("ident: " + splitLine(1), nodeNum)
         
         //Parse AS
         if(splitLine(2).equals("as")) parseTerminal("AS", nodeNum)
         else throw ParseError
         
         //Parse the type of the variable
         //Store the type in an ASTNode, and add it to the subtree of declaration node created earlier
         val typeNode = parseType(NodeList.size, nodeNum)
         declNode.addNode(typeNode)
         
         //Parse the semicolon at the end of the statement
         if(splitLine(4).equals(";")) parseTerminal(";", nodeNum)
         else throw ParseError
         
         
         //Parse a new declarations symbol
         newList = parseDeclarations(NodeList.size, nodeNum)
         newList.add(declNode)
       }
       
       newList
     }
     
     //Parse the type of the variable, and add it to the list; INTs and BOOLs only
     def parseType(nodeNum:Int, parentNum:Int):ASTNode = {
       NodeList.add(new Node("type", parentNum))
       
       //If there is a valid type, then parse it and add the variable, else throw parse error
       if(splitLine(3).equals("int") || splitLine(3).equals("bool")){
         parseTerminal(splitLine(3).toUpperCase, nodeNum)
         varList.add(new Variable(splitLine(1), splitLine(3)))
       }
       else throw ParseError
       
       //Create ASTNode with name of type
       val newNode = new ASTNode(splitLine(3))
       newNode
     }
     
     
     //Parses the statement sequence symbol
     def parseStatementSequence(nodeNum:Int, parentNum:Int):ArrayList[ASTNode] = {
       nextLine()
       NodeList.add(new Node("statementSequence", parentNum))
       var newList = new ArrayList[ASTNode]
       var statement = new ASTNode("blank")
       //Console.println("line")
       //for(i <- 0 to splitLine.size - 1) Console.println(splitLine(i).equals(""))
       
       //If we reach end, print empty string (2nd case is for end of a for loop, when "end" would be preceded by some whitespace
       if(currentLine.equals("end") || splitLine(0).equals("end") || splitLine(0).equals("else")) parseTerminal("&#949;", nodeNum)
       
       else{
         statement = parseStatement(NodeList.size, nodeNum)
         parseTerminal(";", nodeNum)
         newList = parseStatementSequence(NodeList.size, nodeNum)
         newList.add(statement)
       }
       
       newList
     }
     
     //parse a statement
     def parseStatement(nodeNum:Int, parentNum:Int):ASTNode = {
       NodeList.add(new Node("statement", parentNum))
       var statementNode = new ASTNode("")
       //If 2nd term is :=, we take it for an assignment
       if(splitLine(1).equals(":=")){statementNode = parseAssignment(NodeList.size, nodeNum)} 
       
       //if 1st term is if, parse an if statement
       else if(splitLine(0).equals("if")) {statementNode = parseIf(NodeList.size, nodeNum)}
       
       //if 1st term is while, parse a while loop
       else if(splitLine(0).equals("while")){statementNode = parseWhile(NodeList.size, nodeNum)} 
       
       //if 1st term is writeInt, parse writeInt
       else if(splitLine(0).equals("writeInt")){statementNode = parseWriteInt(NodeList.size, nodeNum)} 
       
       statementNode
     }
     
     //parse an assignment
     def parseAssignment(nodeNum:Int, parentNum:Int):ASTNode = {
       NodeList.add(new Node("assignment", parentNum))
       
       
       //Checks if the first argument is a valid variable
       //if(varExists(splitLine(1))) parseTerminal("ident: " + splitLine(1), nodeNum)
       //else throw ParseError
       
       parseTerminal("ident: " + splitLine(0), nodeNum)
       
       //2nd argument has already been checked, so just parse :=
       parseTerminal(":=", nodeNum)
       
       var assignNode = parseAssignment2(NodeList.size, nodeNum)
       assignNode
     }
     
     //2nd part of an assignment parsing; parse readInt or an expression
     def parseAssignment2(nodeNum:Int, parentNum:Int):ASTNode = {
       NodeList.add(new Node("assignment\'", parentNum))
       var assignNode = new ASTNode("") //return node
       
       //Check for readInt
       if(splitLine(2).equals("readInt")){
         parseTerminal("readInt", nodeNum)
         assignNode = new ASTNode(":= readInt")
         
         //add the var at position 0 to the assignNode
         assignNode.addNode(new ASTNode(splitLine(0)))
         
         //NOTE:This functionally will probably be moved to type checking
         /*//Only parses an int
         if(varExists(splitLine(0)).equals("int")) parseTerminal("readInt", nodeNum)
         
         else{
           Console.err.println("Tried to assign a bool to an int")
           throw ParseError
         }*/
       } 
       
       else{
         assignNode = new ASTNode(":=")
         assignNode.addNode(new ASTNode(splitLine(0))) //adds the variable to be assigned
         var expression = parseExpression(NodeList.size(), nodeNum, Arrays.copyOfRange(splitLine, 2, getEndIndex(splitLine, 2)))
         assignNode.addNode(expression)
         }
       assignNode
     }
     
     //parse an if statement
     def parseIf(nodeNum:Int, parentNum:Int):ASTNode = {
       NodeList.add(new Node("assignment\'", parentNum))
       var ifNode = new ASTNode("if")
       //We've already verified the first symbol is if, so parse that as a terminal
       parseTerminal("IF", nodeNum)
       
       
       
       if(splitLine(splitLine.size - 1).equals("then")){
         //If the line ends with "then," parse the expression (every symbol between if and then)
         val endIndex = getEndIndex(splitLine, 1)
         var expressionNode = parseExpression(NodeList.size, nodeNum, Arrays.copyOfRange(splitLine, 1, endIndex))
         ifNode.addNode(expressionNode)

         parseTerminal("THEN", nodeNum)
         
         //Parse as a statement sequence each line between this one and a line beginning with "end" or "else"
         var newList = parseStatementSequence(NodeList.size, nodeNum)
         
         //make an abstract list of statements
         var sequenceNode = new ASTNode("stmt list")
         for(i <- newList.size - 1 to 0 by -1) sequenceNode.addNode(newList.get(i))
         ifNode.addNode(sequenceNode)
         
         
         //attempt to parse an else clause
         var elseNode = parseElse(NodeList.size, nodeNum)
         
         if(!elseNode.name().equals("blank")){ifNode.addNode(elseNode)}
         
         parseTerminal("END", nodeNum)
       } 
       else{
         Console.err.println("then not detected in if statement")
         throw ParseError
       }
       ifNode
     }
     
     //parse an else statement
     def parseElse(nodeNum:Int, parentNum:Int):ASTNode = {
       NodeList.add(new Node("elseclause", parentNum))
       var elseNode = new ASTNode("blank")
       //if there's an else statement, then we parse everything between it and "end" as a 
       if(splitLine(0).equals("else")){
         elseNode = new ASTNode("else")
         parseTerminal("ELSE", nodeNum)
         
         var sequence = parseStatementSequence(NodeList.size, nodeNum)
         var sequenceNode = new ASTNode("stmt list")
         for(i <- sequence.size - 1 to 0 by -1) sequenceNode.addNode(sequence.get(i))
         
         elseNode.addNode(sequenceNode)
       }
       
       //If there's no else (if statement ended with "end," parse an empty string
       else if(splitLine(0).equals("end")) parseTerminal("&#949;", nodeNum)
       
       elseNode
     }
     
     //parse a while statement
     def parseWhile(nodeNum:Int, parentNum:Int):ASTNode = {
       NodeList.add(new Node("whileStatement", parentNum))
       var whileNode = new ASTNode("while")
       //We checked that the first term is while, so we parse that
       parseTerminal("WHILE", nodeNum)
       
       if(splitLine(splitLine.size - 1).equals("do")){
         //If the line ends with "do," parse the expression (every symbol between if and do)
         val endIndex = getEndIndex(splitLine, 1)
         var expressionNode = parseExpression(NodeList.size, nodeNum, Arrays.copyOfRange(splitLine, 1, endIndex))
         whileNode.addNode(expressionNode)
         parseTerminal("DO", nodeNum)
         
         
         //Parse as a statement sequence each line between this one and a line beginning with "end"
         var sequence = parseStatementSequence(NodeList.size, nodeNum)
         var sequenceNode = new ASTNode("stmt list")
         for(i <- sequence.size - 1 to 0 by -1) sequenceNode.addNode(sequence.get(i))
         whileNode.addNode(sequenceNode)
         
         
         
         parseTerminal("END", nodeNum)
       } 
       else{
         Console.err.println("No do statement detected in while declaration")
         throw ParseError
       }
       whileNode
     }
     
     //parse a write int
     def parseWriteInt(nodeNum:Int, parentNum:Int):ASTNode = {
       NodeList.add(new Node("writeInt", parentNum))
       var writeIntNode = new ASTNode("writeInt")
       parseTerminal("WRITEINT", nodeNum)
       
       val endIndex = getEndIndex(splitLine, 2)
       var expressionNode = parseExpression(NodeList.size(), nodeNum, Arrays.copyOfRange(splitLine, 1, endIndex))
       
       writeIntNode.addNode(expressionNode)
       writeIntNode
     }
     
     //parses an expression; for this, we take an array that is only part of the current line, which contains the expression
     def parseExpression(nodeNum:Int, parentNum:Int, expression:Array[String]):ASTNode = {
       NodeList.add(new Node("expression", parentNum))
       var expressionNode = new ASTNode("") //return node
       var simpleExpression = new ASTNode("")
       var expression2 = new ASTNode("")
       
       /*for(i <- 0 to expression.size - 1)
         Console.println(expression(i))
         
         Console.println(expression(1).equals("*"))*/
         
         //checks if the expression has a comparator
         val compareIndex = hasCompare(expression)
         
         if(compareIndex != -1){
           //If the comparator is at the end of the line, then it won't work
           if(compareIndex == 0 || compareIndex == expression.size - 1){
             Console.err.println("Comparator at end of line")
             throw ParseError
           }
           
           expressionNode = new ASTNode(expression(compareIndex))
           
           //Left-recursive code
           /*expression2 = parseExpression (NodeList.size, nodeNum, Arrays.copyOfRange(expression, 0, compareIndex))
           parseTerminal(expression(compareIndex), nodeNum)
           simpleExpression = parseSimpleExpression(NodeList.size, nodeNum, Arrays.copyOfRange(expression, compareIndex + 1, expression.size))
           
           expressionNode.addNode(expression2)
           expressionNode.addNode(simpleExpression)*/
           //
           
           simpleExpression = parseSimpleExpression(NodeList.size, nodeNum, Arrays.copyOfRange(expression, 0, compareIndex))
           expressionNode.addNode(simpleExpression)
         }
         else expressionNode = parseSimpleExpression(NodeList.size, nodeNum, expression)
         
         expression2 = parseExpression2(NodeList.size, nodeNum, expression)         
         if(!expression2.name().equals("blank")){expressionNode.addNode(expression2)} //adds the second half only if there is one
       
         
         expressionNode
     }
     
     //If there is a compare, parse the expression to the right of it. Otherwise, parse an empty string
     def parseExpression2(nodeNum:Int, parentNum:Int, expression:Array[String]):ASTNode = {
       NodeList.add(new Node("expression\'", parentNum))
       var expressionNode = new ASTNode("blank") //return value (blank by default, unless empty string is not parsed)
       
       //checks if the expression has an additive
       val compareIndex = hasCompare(expression)
       if(compareIndex != -1){
         //If the comparator is at the end of the line, then it won't work
           if(compareIndex == 0 || compareIndex == expression.size - 1){
             Console.err.println("Comparator at end of line")
             throw ParseError
           }
           
           parseTerminal(expression(compareIndex), nodeNum)
           expressionNode = parseExpression(NodeList.size, nodeNum, Arrays.copyOfRange(expression, compareIndex + 1, expression.size))
       }
       else parseTerminal("&#949;", nodeNum)
       
       expressionNode
     }
     
     //parse a simple expression; split expression on additive
     def parseSimpleExpression(nodeNum:Int, parentNum:Int, expression:Array[String]):ASTNode = {
       NodeList.add(new Node("simpleExpression", parentNum))
       var simpleExpressionNode = new ASTNode("") //return node
       var term = new ASTNode("")
       var simpleExpression2 = new ASTNode("")
       
       //checks if the expression has an additive
         val addIndex = hasAdditive(expression)
         if(addIndex != -1){
           //If the comparator is at the end of the line, then it won't work
           if(addIndex == 0 || addIndex == expression.size - 1){
             Console.err.println("Additive at end of line")
             throw ParseError
           }
           
           simpleExpressionNode = new ASTNode(expression(addIndex))
           
           
           //Left-recursive code
           /*simpleExpression2 = parseSimpleExpression(NodeList.size, nodeNum, Arrays.copyOfRange(expression, 0, addIndex))
           parseTerminal(expression(addIndex), nodeNum)
           term = parseTerm(NodeList.size, nodeNum, Arrays.copyOfRange(expression, addIndex + 1, expression.size))
           
           simpleExpressionNode.addNode(simpleExpression2)
           simpleExpressionNode.addNode(term)*/
           //
           
           term = parseTerm(NodeList.size, nodeNum, Arrays.copyOfRange(expression, 0, addIndex))   
           simpleExpressionNode.addNode(term)
         }
         else simpleExpressionNode = parseTerm(NodeList.size, nodeNum, expression)
         
         simpleExpression2 = parseSimpleExpression2(NodeList.size, nodeNum, expression)
         if(!simpleExpression2.name().equals("blank")){simpleExpressionNode.addNode(simpleExpression2)} //adds the second half only if there is one
       
         simpleExpressionNode
     }
     
     def parseSimpleExpression2(nodeNum:Int, parentNum:Int, expression:Array[String]):ASTNode = {
       NodeList.add(new Node("simpleExpression\'", parentNum))
       var simpleExpressionNode = new ASTNode("blank") //return value (blank by default, unless empty string is not parsed)
       
       //checks if the expression has an additive
       val addIndex = hasAdditive(expression)
       if(addIndex != -1){
         //If the comparator is at the end of the line, then it won't work
           if(addIndex == 0 || addIndex == expression.size - 1){
             Console.err.println("Additive at end of line")
             throw ParseError
           }
           
           parseTerminal(expression(addIndex), nodeNum)
           simpleExpressionNode = parseSimpleExpression(NodeList.size, nodeNum, Arrays.copyOfRange(expression, addIndex + 1, expression.size))
       }
       else parseTerminal("&#949;", nodeNum)

       simpleExpressionNode
     }
     
     //parse term; split on multiplicatives
     def parseTerm(nodeNum:Int, parentNum:Int, expression:Array[String]):ASTNode = {
       NodeList.add(new Node("term", parentNum))
       var termNode = new ASTNode("") //return node
       var factor = new ASTNode("")
       var term2 = new ASTNode("")
       val mulIndex = hasMultiplicative(expression)
       if(mulIndex != -1){
           //If the comparator is at the end of the line, then it won't work
           if(mulIndex == 0 || mulIndex == expression.size - 1){
             Console.err.println("Multiplicative at end of line")
             throw ParseError
           }
           termNode = new ASTNode(expression(mulIndex))
           
           //Left-recursive code
           /*term2 = parseTerm(NodeList.size, nodeNum, Arrays.copyOfRange(expression, 0, mulIndex))
           
           
           parseTerminal(expression(mulIndex), nodeNum)
           
           factor = parseFactor(NodeList.size, nodeNum, Arrays.copyOfRange(expression, mulIndex + 1, expression.size))
           
           termNode.addNode(term2)
           termNode.addNode(factor)*/
           //
           
           factor = parseFactor(NodeList.size, nodeNum, Arrays.copyOfRange(expression, 0, mulIndex))
           termNode.addNode(factor)
       }
       else termNode = parseFactor(NodeList.size, nodeNum, expression)
       
       term2 = parseTerm2(NodeList.size, nodeNum, expression) //parse the second half of the expression
       if(!term2.name().equals("blank")){termNode.addNode(term2)} //adds the second half only if there is one
       
       termNode
     }
     
     def parseTerm2(nodeNum:Int, parentNum:Int, expression:Array[String]):ASTNode = {
       NodeList.add(new Node("term\'", parentNum))
       var termNode = new ASTNode("blank") //return value (blank by default, unless empty string is not parsed)
       //checks if the expression has a multiplicative, then parses everything after it
       val mulIndex = hasMultiplicative(expression)
       if(mulIndex != -1){
         //If the comparator is at the end of the line, then it won't work
           if(mulIndex == 0 || mulIndex == expression.size - 1){
             Console.err.println("Additive at end of line")
             throw ParseError
           }
           
           parseTerminal(expression(mulIndex), nodeNum)
           termNode = parseTerm(NodeList.size, nodeNum, Arrays.copyOfRange(expression, mulIndex + 1, expression.size))
       }
       else parseTerminal("&#949;", nodeNum)
       
       termNode
     }
     
     //parse factor
     def parseFactor(nodeNum:Int, parentNum:Int, expression:Array[String]):ASTNode = {
       NodeList.add(new Node("factor", parentNum))
       var factorNode = new ASTNode("") //return value
       //if the factor is a single term
       if(expression.size == 1){
         val factor = expression(0)
         
         //checks if the factor is a num
         if(factor.matches("[1-9][0-9]*|0")) parseTerminal("num: " + factor, nodeNum)
         
         //checks if the factor is a boolit
         else if(factor.matches("false|true")) parseTerminal("boolit: " + factor, nodeNum)
         
         //checks if the factor is an ident
         else if(factor.matches("[A-Z][A-Z0-9]*")) parseTerminal("ident: " + factor, nodeNum)
         
         //if factor doesn't match a regex
         else{
           Console.err.println(factor + " did not match a regex")
           throw ParseError
         }
         
         //set factorNode to the new factor
         factorNode = new ASTNode(expression(0))
       }
       //If factor has more than one term, try to parse it as an expression
       else{
         parseTerminal("LP", nodeNum)
         factorNode = parseExpression(NodeList.size, nodeNum, expression)
         parseTerminal("RP", nodeNum)
       }
       //Console.println(expression(0).matches("[1-9][0-9]*|0"))
       factorNode //return
     }
     
     //Parse a terminal symbol with given text
     def parseTerminal(text:String, parentNum:Int):ASTNode = {
       NodeList.add(new Node(text, parentNum))
       
       //Make and return ASTNode
       val newNode = new ASTNode(text)
       newNode
     }
     
     //If a given var exists in varList, return its type
     def varExists(varName:String):String = {
       var varType = "none"
       for(i <- 0 to varList.size - 1){
         if(varList.get(i).getName().equals(varName))
           varType = varList.get(i).getType()
       }
       varType
     }
     
     //Determines whether or not a given array of strings contains an additive; returns index of additive, or -1 otherwise
     def hasAdditive(stringList:Array[String]):Int = {
       var index = -1
       for(i <- stringList.size - 1 to 0 by -1){
         if(stringList(i).equals("+") || stringList(i).equals("-")){
           if(index == -1) index = i //Only reassigns index once (returns first instance)
         }
       }
       
       index
     }
     
     //Determines whether or not a given array of strings contains a multiplicative; returns index of multiplicative, or -1 otherwise
     def hasMultiplicative(stringList:Array[String]):Int = {
       var index = -1
       for(i <- stringList.size - 1 to 0 by -1){
         if(stringList(i).equals("*") || stringList(i).equals("div") || stringList(i).equals("mod")){
           if(index == -1) index = i
           }
         
       }
      
       index
     }
     
     //Determines whether or not a given array of strings contains a multiplicative; returns index of multiplicative, or -1 otherwise
     def hasCompare(stringList:Array[String]):Int = {
       var index = -1
       for(i <- stringList.size - 1 to 0 by -1){
         if(stringList(i).equals("=") || stringList(i).equals("!=") || stringList(i).equals("<") || stringList(i).equals(">") || stringList(i).equals("<=") || stringList(i).equals(">=")){
           if(index == -1) index = i
         }
       }
       
       index
     }
     
     //reads the array until we hit the end of the line, "do", or a semicolon, and returns that index
     def getEndIndex(stringList:Array[String], startIndex:Int):Int ={
       for(i <- startIndex to stringList.size - 1){
         if(stringList(i).equals("do") || stringList(i).equals(";")) i - 1
       }
       
       stringList.size - 1
     }
     
     //removes any blanks in splitLine
     def removeBlanks(){
       var blankIndex = 0
       
       for(i <- 0 to splitLine.size - 1){
         if(!splitLine(i).equals("") && blankIndex == 0) blankIndex = i
       }
       
       splitLine = Arrays.copyOfRange(splitLine, blankIndex, splitLine.size)
     }
     
     
     //Goes to the next line; skips up to the end of the file before throwing a parse error, then splits the line
     def nextLine(){
       if(lineNum == lines.size() - 1){
         Console.err.println("Reading past end of file")
         throw ParseError
       }
       
       lineNum = lineNum + 1
       var count = 0
       //Console.println(lines.get(lineNum).matches("""\s"""))
       /*while(lines.get(lineNum).matches("""\s""") && lineNum < lines.size() - 1){
         lineNum = lineNum + 1
       }
       
       //At end of loop, if line is still empty, throw parse error
       if(lines.get(lineNum).equals("")) throw ParseError*/
       
       currentLine = lines.get(lineNum)
       splitLine = currentLine.split("""\s""") //splits the current line by whitespace
       removeBlanks() //removes leading whitespace
       if(splitLine(0).equals("")) nextLine() //goes to next line if this one is blank
     }
     
     //Prints all nodes and their parents
     def printNodes(){
       Console.println("Node List:")
       for(i <- 0 to NodeList.size - 1){
         Console.println("Text: " + NodeList.get(i).getText());
         Console.println("Parent: n" + NodeList.get(i).getParent())
       }
     }
     
     //Writes the contents of the node list to a graph file
     def writeToFile(){
       val writer = new java.io.PrintWriter(new File(fileName))
       
       writer.println("digraph g {")
       //Write the first node
       writer.println("n0 [label=\"" + NodeList.get(0).getText + "\"]")
       
       //Write the other nodes
       for(i <- 1 to NodeList.size - 1){
         writer.println("n" + i + " [label=\"" + NodeList.get(i).getText + "\"]")
         writer.println("n" + NodeList.get(i).getParent() + " -> n" + i)
       }
       
       writer.println("}")
       writer.close()
     }
}