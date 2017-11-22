package edu.utsa.cs5363

import java.util.ArrayList
import java.io.File

class MIPSGenerator(ILOCBlocks:ArrayList[InstructionBlock], reg:Map[String,Int], fileName:String) {
    var instrs = new ArrayList[MIPSInstruction]
   
    def addInstr(name:String){
      instrs.add(new MIPSInstruction(name))
      
    }
    
    
    //MAIN METHOD:Generates a set of MIPS instructions using the ILOC instructions
    def genMIPS(){
      //iterate through each block
      for(i <- 0 to ILOCBlocks.size() - 1){
        addInstr("b" + i + ":") //Declare a new block in the MIPS code
        val currentBlock = ILOCBlocks.get(i)
        
        //iterate through each instruction in the block
        for(j <- 0 to currentBlock.size() - 1){
          val currentInst = currentBlock.getInst(j)
          val instType = currentInst.getType()
          
          addInstr("# " + currentInst.getName())
          //generate the relevant series of instructions based on the type of the ILOC instruction
          instType match{
            case "loadI" => genLoadI(currentInst)
            case "readInt" => genReadInt(currentInst)
            case "add" => genAdd(currentInst)
            case "sub" => genSub(currentInst)
            case "mult" => genMult(currentInst)
            case "div" => genDiv(currentInst)
            case "mod" => genMod(currentInst)
            case "cmp_EQ" => genCmpEQ(currentInst)
            case "cmp_NE" => genCmpNE(currentInst)
            case "cmp_GT" => genCmpGT(currentInst)
            case "cmp_GTE" => genCmpGTE(currentInst)
            case "cmp_LT" => genCmpLT(currentInst)
            case "cmp_LTE" => genCmpLTE(currentInst)
            case "cbr" => gencbr(currentInst)
            case "writeInt" => genWriteInt(currentInst)
            case "i2i" => geni2i(currentInst)
            case "jump" => genJump(currentInst)
            case "exit" => genExit(currentInst)
            case _ => Console.println(instType)
          }
          addInstr("")
        }
      }
      
      
      writeToFile()
    }
    
    //Instruction methods: generates a series of MIPS instructions from a specific type of ILOC instruction/////////////
    
    //Relevant fields: in1(immediate), out1(reg)
    def genLoadI(instr:Instruction){
      val offset = reg(instr.getOut1()) //Need offset of the output register
      
      addInstr("li $t0, " + instr.getIn1())
      addInstr("sw $t0, " + offset + "($fp)")
    }
    
    //Relevant fields: in1(reg)
    def genReadInt(instr:Instruction){
      val offset = reg(instr.getIn1())
      
      addInstr("li $v0, 5")
      addInstr("syscall")
      addInstr("add $t0, $v0, $zero")
      addInstr("sw $t0, " + offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genAdd(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("addu $t0, $t1, $t2")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genSub(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("subu $t0, $t1, $t2")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genMult(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("mul $t0, $t1, $t2")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genDiv(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("divu $t1, $t2")
      addInstr("mflo $t0")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genMod(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("divu $t1, $t2")
      addInstr("mfhi $t0")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genCmpEQ(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("seq $t0, $t1, $t2")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genCmpNE(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("sne $t0, $t1, $t2")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genCmpGT(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("sgt $t0, $t1, $t2")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genCmpGTE(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("sge $t0, $t1, $t2")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genCmpLT(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("slt $t0, $t1, $t2")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), in2(reg), out1(reg)
    def genCmpLTE(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val in2Offset = reg(instr.getIn2())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("lw $t2, " + in2Offset + "($fp)")
      addInstr("sle $t0, $t1, $t2")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(reg), out1(block), out2(block
    def gencbr(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val out1 = instr.getOut1()
      val out2 = instr.getOut2()
      
      addInstr("lw $t0, " + in1Offset + "($fp)")
      addInstr("bne $t0, $zero, " + out1)
      addInstr("j " + out2)
    }
    
    //Relevant fields: in1(reg)
    def genWriteInt(instr:Instruction){
      val offset = reg(instr.getIn1())
      
      addInstr("li $v0, 1")
      addInstr("lw $t1, " + offset + "($fp)")
      addInstr("add $a0, $t1, $zero")
      addInstr("syscall")
      addInstr("li $v0, 4")
      addInstr("la $a0, newline")
      addInstr("syscall")
    }
    
    //Relevant fields: in1(reg), out1(reg)
    def geni2i(instr:Instruction){
      val in1Offset = reg(instr.getIn1())
      val out1Offset = reg(instr.getOut1())
      
      addInstr("lw $t1, " + in1Offset + "($fp)")
      addInstr("add $t0, $t1, $zero")
      addInstr("sw $t0, " + out1Offset + "($fp)")
    }
    
    //Relevant fields: in1(block)
    def genJump(instr:Instruction){
      val in1 = instr.getIn1()
      
      addInstr("j " + in1)
    }
    
    //Relevant fields: none
    def genExit(instr:Instruction){
      addInstr("li $v0, 10")
      addInstr("syscall")
    }
    
    //////////////////////////////////////////////////////////////////////////////////////////////
    
    //Prints the list of MIPS instructions to a file
    def writeToFile(){
      val writer = new java.io.PrintWriter(new File(fileName))
      
      //print the header for the file
      writer.println("\t.data")
      writer.println("newline:\t.asciiz \"\\n\"")
      writer.println("\t.text")
      writer.println("\t.globl main")
      writer.println("main:")
      writer.println("\t li $fp, 0x7ffffffc")
      
      //print the MIPS instructions
      for(i <- 0 to instrs.size - 1){
        writer.println(instrs.get(i).getName())
      }
      
      writer.close()
    }
}