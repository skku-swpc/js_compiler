# Generated from ECMAScript.g4 by ANTLR 4.5.1
import sys
import traceback
import os

from antlr4 import *

from llvmlite.ir import *

noinfered_ = -2
failed_ = -1
null_ = 0
bool_ = 1
int_ = 2
float_ = 3
string_ = 4
int_array_ = 5
float_array_ = 6
string_array_ = 7
obj_ = 8
function_ = 9
unknown = sys.maxint

module = Module (name = 'wow')

# This class defines a complete generic visitor for a parse tree produced by ECMAScriptParser.

class ECMAScriptLLVMEmitter(ParseTreeVisitor):

    def __init__ (self, graph, filename):
        self.nodeRoot = graph
        self.idx = -1
        self.currentF = 'global'
        self.path = filename
        self.symTableF = {}

    def recLeaves (self, leaf, endLeaves, visit = []):
        ret = []
        if leaf in visit:
            return []
        visit.append (leaf)
        if leaf in endLeaves:
            return [leaf]
        if leaf.getChild () is None or leaf.getChild () == []:
            return [leaf]

        for cld in leaf.getChild ():
            ret.extend (self.recLeaves (cld, end_leaves, visit))
        return leaves

    def llvmType (self, ty, elTy = None):
        if ty == int_:
            return IntType (32)
        elif ty == float_:
            return FloatType ()
        elif ty == string_:
            return PointerType (IntType (8))
        elif ty == int_array_:
            return PointerType (IntType (32))
        elif ty == float_array_:
            return PointerType (FloatType ())
        elif ty == string_array_:
            return PointerType (PointerType (IntType (8)))
        elif ty == obj_:
            return LiteralStructType (elTy)
        else:
            print 'unknown type occurs (' + str (ty) + ')'
            return None

    def asmType (self, ty):
        if ty == type (IntType (32)):
            return int_
        elif ty == type (FloatType ()):
            return float_
        elif ty == type (PointerType (IntType (8))):
            return string_
        elif ty == type (PointerType (IntType (32))):
            return int_array_
        else:
            print 'unexpected data type occurs! (' + str (ty) + ')'
            return None

    def getVal (self, cand, leaf):
        cached = leaf.getInst (cand)
        if cached is not None:
            return cached
        builder = IRBuilder (leaf.getBB ())
        flag = False
        if hasattr (cand, 'opcode_name'):
            if cand.opcode_name == 'getelementptr':
                flag = True
        if type (cand) == AllocaInstr or flag is True:
            load = builder.load (cand)
            leaf.cacheInst (cand, load)
            return load
        else:
            return cand

    def setVal (self, val, cand, leaf):
        builder = IRBuilder (leaf.getBB ())
        flag = False
        if type (cand) is int or type (cand) is float:
            return cand
        if hasattr (cand, 'opcode_name'):
            if cand.opcode_name == 'getelementptr':
                flag = True
        if type (cand) is AllocaInstr or flag is True:
            store = builder.store (self.getVal (val, leaf), cand)
            leaf.cacheInst (val, cand)
        else:
            store = self.getVal (val, leaf)
        return store

    def funcType (self, nodeRoot):
        tyF = []

        #for leaf, call, struct in zip (nodeRoot['root'], nodeRoot['call'], nodeRoot['struct'].itervalues ()):
        for leaf, call in zip (nodeRoot['root'], nodeRoot['call']):
            tyArg = []
            for arg in leaf.getArg ():
                tyArg.append (self.llvmType (arg))
            ty = self.llvmType (call)
            if call != obj_:
                tyF.append (FunctionType (ty, tyArg))
            '''else:
                structTy = {}
                for ty in struct.itervalues ():
                    if ty != function_ or ty.type is not str:
                        structTy.append (self.llvmType (ty))
                    else:
                        pass
                    #struct_markup
                struct[str (leaf.getArg ())] = LiteralStruct (structTy, 'struct.' + self.currentF)
                tyF.append (FunctionType (struct[str (leaf.getArg())], tyArg))'''
        return tyF

    # Visit a parse tree produced by ECMAScriptParser#program.
    def visitProgram(self, ctx):
        module.data_layout = 'e-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-p:32:32:32-v128:32:128-n32-S128'
        module.triple = 'asmjs-unknown-emscripten'
        self.visitChildren(ctx)

        if os.path.isdir (self.path):
            fileName = self.path + '/wow.ll'
        else:
            fileName = self.path
        f = open (fileName, 'w')
        f.write (str (module))
        f.close ()


    # Visit a parse tree produced by ECMAScriptParser#sourceElements.
    def visitSourceElements(self, ctx):
        if hasattr (ctx, 'leaves'):
            elems = ctx.sourceElement ()
            for elem in elems:
                elem.leaves = ctx.leaves
                self.visit (elem)
        else:
            self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#sourceElement.
    def visitSourceElement(self, ctx):
        if hasattr (ctx, 'leaves'):
            stmt = ctx.statement ()
            stmt.leaves = ctx.leaves
            self.visit (stmt)
        else:
            self.visitChildren (ctx)


    # Visit a parse tree produced by ECMAScriptParser#statement.
    def visitStatement(self, ctx):
        child = ctx.getChild (0)
        child.leaves = ctx.leaves
        self.visit (child)


    # Visit a parse tree produced by ECMAScriptParser#block.
    def visitBlock(self, ctx):
        stmtList = ctx.statementList ()
        stmtList.leaves = ctx.leaves
        self.visit (stmtList)


    # Visit a parse tree produced by ECMAScriptParser#statementList.
    def visitStatementList(self, ctx):
        stmts = ctx.statement ()

        for stmt in stmts:
            stmt.leaves = ctx.leaves
            self.visit (stmt)


    # Visit a parse tree produced by ECMAScriptParser#variableStatement.
    def visitVariableStatement(self, ctx):
        varDeclList = ctx.variableDeclarationList ()
        varDeclList.leaves = ctx.leaves
        self.visit (varDeclList)


    # Visit a parse tree produced by ECMAScriptParser#variableDeclarationList.
    def visitVariableDeclarationList(self, ctx):
        decls = ctx.variableDeclaration ()

        for decl in decls:
            decl.leaves = ctx.leaves
            self.visit (decl)


    # Visit a parse tree produced by ECMAScriptParser#variableDeclaration.
    def visitVariableDeclaration(self, ctx):
        var = str (ctx.Identifier ())
        init = ctx.initialiser ()

        init.leaves = ctx.leaves
        self.visit (init)

        for leaf, assigner in zip (ctx.leaves, init.value):
            assignee = leaf.getSym (var)
            assignee_val = self.getVal (var, leaf)
            assigner_val = None
            if not hasattr (assigner, 'isArray'):
                assigner_val = self.getVal (assigner, leaf)

            builder = IRBuilder (leaf.getBB ())
            if type (assignee) is str:
                if hasattr (assigner, 'isArray'):
                    alloc = assigner
                    alloc.name = assignee
                else:
                    alloc = builder.alloca (assigner_val.type, name = var)
                leaf.setSym (var, alloc)
            else:
                if assigner_val.type == assignee_val.type:
                    alloc = var
                else:
                    alloc = builder.alloca (assigner.type, name = var)
                    leaf.symSym (name, alloc)


    # Visit a parse tree produced by ECMAScriptParser#initialiser.
    def visitInitialiser(self, ctx):
        single = ctx.singleExpression ()
        single.leaves = ctx.leaves
        self.visit (single)
        ctx.value = single.value


    # Visit a parse tree produced by ECMAScriptParser#emptyStatement.
    def visitEmptyStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#expressionStatement.
    def visitExpressionStatement(self, ctx):
        seq = ctx.expressionSequence ()
        seq.leaves = ctx.leaves
        self.visit (seq)
        ctx.value = seq.value


    # Visit a parse tree produced by ECMAScriptParser#ifStatement.
    def visitIfStatement(self, ctx):
        seq = ctx.expressionSequence ()
        stmt = ctx.statement ()
        ifThenLeaves = []
        ifElseLeaves = []

        seq.leaves = ctx.leaves
        self.visit (seq)

        for leaf, cond in zip (ctx.leaves, seq.value):
            clds = leaf.getChild ()
            func = leaf.getFunc ()
            funcName = leaf.getFuncName ()
            builder = IRBuilder (leaf.getBB ())
            if_then = None
            if_else = None
            for cld in clds:
                cld.inheritSymTable (leaf.getSymTable ())
                cld.setFunc (func, funcName)
                if cld.isIf ():
                    cld.setBB (func.append_basic_block ('if_then'))
                    #IRBuilder (cld.getBB ()).if_then (leaf.getBB ())
                    ifThenLeaves.append (cld)
                    if_then = cld.getBB ()
                else:
                    cld.setBB (func.append_basic_block ('if_else'))
                    #IRBuilder (cld.getBB ()).if_else (leaf.getBB ())
                    ifElseLeaves.append (cld)
                    if_else = cld.getBB ()
            c = builder.cbranch (cond[0], if_then, if_else)

        del ctx.leaves[:]
        stmt[0].leaves = ifThenLeaves
        self.visit (stmt[0])
        ctx.leaves.extend (ifThenLeaves)
        if stmt[0] != stmt[-1]:
            stmt[1].leaves = ifElseLeaves
            self.visit (stmt[1])
        ctx.leaves.extend (ifElseLeaves)


    # Visit a parse tree produced by ECMAScriptParser#DoStatement.
    def visitDoStatement(self, ctx):
        seq = ctx.expressionSequence ()
        stmt = ctx.statement ()

        iterLeaves = []
        nextLeaves = []

        for leaf in ctx.leaves:
            builder = IRBuilder (leaf.getBB ())
            cld = leaf.getChild ()[0]
            func = leaf.getFunc ()
            funcName = leaf.getFuncName ()
            cld.inheritSymTable (leaf.getSymTable ())
            cld.setFunc (func, funcName)
            cld.setBB (func.append_basic_block (name = 'iter_body'))
            builder.branch (cld.getBB ())
            iterLeaves.append (cld)

        del ctx.leaves[:]
        ctx.leaves.extend (iterLeaves)
        stmt.leaves = ctx.leaves
        self.visit (stmt)
        seq.leaves = ctx.leaves
        self.visit (seq)

        for leaf, cond in zip (ctx.leaves, seq.value):
            builder = IRBuilder (leaf.getBB ())
            clds = leaf.getChild ()
            func = leaf.getFunc ()
            funcName = leaf.getFuncName ()
            if_then = None
            if_else = None

            for cld in clds:
                cld.inheritSymTable (leaf.getSymTable ())
                cld.setFunc (func, funcName)
                if cld.isIf ():
                    #IRBuilder (cld.getBB ()).if_then (leaf.getBB ())
                    if_then = cld.getBB ()
                else:
                    cld.setBB (func.append_basic_block (name = 'elem'))
                    #IRBuilder (cld.getBB ()).if_else (leaf.getBB ())
                    if_else = cld.getBB ()
                    nextLeaves.append (cld)
            builder.cbranch (cond[0], if_then, if_else)

        del ctx.leaves[:]
        ctx.leaves.extend (nextLeaves)


    # Visit a parse tree produced by ECMAScriptParser#WhileStatement.
    def visitWhileStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#ForStatement.
    def visitForStatement(self, ctx):
        seq = ctx.expressionSequence ()
        stmt = ctx.statement ()
        condLeaves = []
        nextLeaves = []

        seq[0].leaves = ctx.leaves
        self.visit (seq[0])
        seq[1].leaves = ctx.leaves
        self.visit (seq[1])

        for leaf, cond in zip (ctx.leaves, seq[1].value):
            builder = IRBuilder (leaf.getBB ())
            clds = leaf.getChild ()
            func = leaf.getFunc ()
            funcName = leaf.getFuncName ()
            if_then = None
            if_else = None
            for cld in clds:
                cld.inheritSymTable (leaf.getSymTable ())
                cld.setFunc (func, funcName)
                if cld.isIf ():
                    cld.setBB (func.append_basic_block (name = 'iter_body'))
                    #IRBuilder (cld.getBB ()).if_then (leaf.getBB ())
                    if_then = cld.getBB ()
                    condLeaves.append (cld)
                else:
                    cld.setBB (func.append_basic_block (name = 'elem'))
                    #IRBuilder (cld.getBB ()).if_else (leaf.getBB ())
                    if_else = cld.getBB ()
                    nextLeaves.append (cld)
            builder.cbranch (cond[0], if_then, if_else)
        del ctx.leaves[:]
        ctx.leaves.extend (condLeaves)
        stmt.leaves = ctx.leaves
        self.visit (stmt)
        seq[2].leaves = ctx.leaves
        self.visit (seq[2])
        seq[1].leaves = ctx.leaves
        self.visit (seq[1])

        for leaf, cond in zip (ctx.leaves, seq[1].value):
            builder = IRBuilder (leaf.getBB ())
            clds = leaf.getChild ()
            func = leaf.getFunc ()
            funcName = leaf.getFuncName ()
            if_then = None
            if_else = None
            for cld in clds:
                cld.inheritSymTable (leaf.getSymTable ())
                cld.setFunc (func, funcName)
                if cld.isIf ():
                    #IRBuilder (cld.getBB ()).if_then (leaf.getBB ())
                    if_then = cld.getBB ()
                else:
                    cld.setBB (func.append_basic_block (name = 'elem'))
                    #IRBuilder (cld.getBB ()).if_else (leaf.getBB ())
                    if_else = cld.getBB ()
                    nextLeaves.append (cld)
            builder.cbranch (cond[0], if_then, if_else)

        del ctx.leaves[:]
        ctx.leaves.extend (nextLeaves)


    # Visit a parse tree produced by ECMAScriptParser#ForVarStatement.
    def visitForVarStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#ForInStatement.
    def visitForInStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#ForVarInStatement.
    def visitForVarInStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#continueStatement.
    def visitContinueStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#breakStatement.
    def visitBreakStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#returnStatement.
    def visitReturnStatement(self, ctx):
        seq = ctx.expressionSequence ()
        seq.leaves = ctx.leaves
        self.visit (seq)

        for leaf, value in zip (ctx.leaves, seq.value):
            ret = self.getVal (value[0], leaf)
            builder = IRBuilder (leaf.getBB ())

            print self.asmType (type (leaf.getFunc ().type.pointee.return_type)) > self.asmType (type (ret.type))

            if self.asmType (type (leaf.getFunc ().type.pointee.return_type)) > self.asmType (type (ret.type)):
                if type (ret.type) is IntType:
                    if type (ret) is Constant:
                        ret = Constant (FloatType (), float (seq.getText ()))
                    else:
                        ret = builder.sitofp (ret, FloatType ())
                else:
                    ret = builder.bitcast (ret, FloatType ())
            #print leaf.getBB ()
            builder.ret (ret)
            leaf.term = True
        del ctx.leaves[:]


    # Visit a parse tree produced by ECMAScriptParser#withStatement.
    def visitWithStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#switchStatement.
    def visitSwitchStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#caseBlock.
    def visitCaseBlock(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#caseClauses.
    def visitCaseClauses(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#caseClause.
    def visitCaseClause(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#defaultClause.
    def visitDefaultClause(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#labelledStatement.
    def visitLabelledStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#throwStatement.
    def visitThrowStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#tryStatement.
    def visitTryStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#catchProduction.
    def visitCatchProduction(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#finallyProduction.
    def visitFinallyProduction(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#debuggerStatement.
    def visitDebuggerStatement(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#functionDeclaration.
    def visitFunctionDeclaration(self, ctx):
        funcName = str (ctx.Identifier ())
        params = ctx.formalParameterList ()
        funcBody = ctx.functionBody ()

        nodeRoot = self.nodeRoot[funcName]
        ctx.leaves = nodeRoot['root']
        originF = self.currentF
        self.currentF = funcName

        if params is not None:
            paramId = params.Identifier ()
            tyF = self.funcType (nodeRoot)
        else:
            paramId = []
            tyF = [FunctionType (self.llvmType (nodeRoot['call'][0]), [])]

        if self.symTableF.has_key (funcName):
            symT = self.symTableF[funcName]

            for leaf, ty in zip (ctx.leaves, symT):
                f = self.symTableF[funcName][str (ty)]
                leaf.setFunc (f, funcName)

                bb = f.append_basic_block ('function_init')
                leaf.setBB (bb)

                builder = IRBuilder (bb)
                for arg, ty_, farg in zip (paramId, leaf.getArg (), f.args):
                    farg.name = str (arg)
                    alloc = builder.alloca (self.llvmType (ty_), name = farg.name)
                    builder.store (farg, alloc)
                    leaf.setSym (farg.name, alloc)
        else:
            self.symTableF[funcName] = {}
            for leaf, ty, idx in zip (ctx.leaves, tyF, nodeRoot['idx']):
                self.idx += 1

                if funcName == 'main':
                    f = Function (module, ty, name = funcName)
                else:
                    f = Function (module, ty, name = '__' + str (idx))
                self.symTableF[funcName][str (leaf.getArg ())] = f
                leaf.setFunc (f, funcName)
                bb = f.append_basic_block ('function_init')
                leaf.setBB (bb)

                builder = IRBuilder (bb)
                for arg, ty_, farg in zip (paramId, leaf.getArg (), f.args):
                    farg.name = str (arg)
                    alloc = builder.alloca (self.llvmType (ty_), name = farg.name)
                    builder.store (farg, alloc)
                    leaf.setSym (farg.name, alloc)

        funcBody.leaves = ctx.leaves
        self.visit (funcBody)

        self.currentF = originF


    # Visit a parse tree produced by ECMAScriptParser#formalParameterList.
    def visitFormalParameterList(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#functionBody.
    def visitFunctionBody(self, ctx):
        srcElem = ctx.sourceElements ()
        srcElem.leaves = ctx.leaves
        self.visit (srcElem)


    # Visit a parse tree produced by ECMAScriptParser#arrayLiteral.
    def visitArrayLiteral(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#elementList.
    def visitElementList(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#elision.
    def visitElision(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#objectLiteral.
    def visitObjectLiteral(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#propertyNameAndValueList.
    def visitPropertyNameAndValueList(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#PropertyExpressionAssignment.
    def visitPropertyExpressionAssignment(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#PropertyGetter.
    def visitPropertyGetter(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#PropertySetter.
    def visitPropertySetter(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#propertyName.
    def visitPropertyName(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#propertySetParameterList.
    def visitPropertySetParameterList(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#arguments.
    def visitArguments(self, ctx):
        argList = ctx.argumentList ()
        argList.leaves = ctx.leaves
        self.visit (argList)
        ctx.value = argList.value
        ctx.ty = argList.ty


    # Visit a parse tree produced by ECMAScriptParser#argumentList.
    def visitArgumentList(self, ctx):
        ctx.value = []
        ctx.ty = []
        singles = ctx.singleExpression ()

        for single in singles:
            single.leaves = ctx.leaves
            self.visit (single)

        for leaf in ctx.leaves:
            ctx.value.append ([])
            ctx.ty.append ([])

        for single in singles:
            for val, value, ty in zip (single.value, ctx.value, ctx.ty):
                val = self.getVal (val, leaf)
                value.append (val)
                ty.append (self.asmType (type (val.type)))


    # Visit a parse tree produced by ECMAScriptParser#expressionSequence.
    def visitExpressionSequence(self, ctx):
        ctx.value = []
        singles = ctx.singleExpression ()

        for single in singles:
            single.leaves = ctx.leaves
            self.visit (single)
            ctx.value.append (single.value)

        ctx.value = zip (*ctx.value)


    # Visit a parse tree produced by ECMAScriptParser#TernaryExpression.
    def visitTernaryExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#LogicalAndExpression.
    def visitLogicalAndExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#PreIncrementExpression.
    def visitPreIncrementExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#ObjectLiteralExpression.
    def visitObjectLiteralExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#InExpression.
    def visitInExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#LogicalOrExpression.
    def visitLogicalOrExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#NotExpression.
    def visitNotExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#PreDecreaseExpression.
    def visitPreDecreaseExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#ArgumentsExpression.
    def visitArgumentsExpression(self, ctx):
        ctx.value = []
        func = None

        single = ctx.singleExpression ()
        args = ctx.arguments ()

        single.leaves = ctx.leaves
        self.visit (single)
        args.leaves = ctx.leaves
        self.visit (args)

        for leaf, funcName, value, ty in zip (ctx.leaves, single.value, args.value, args.ty):
            builder = IRBuilder (leaf.getBB ())

            if self.symTableF.has_key (funcName):
                func = self.symTableF[funcName][str (ty)]
            else:
                nodeRoot = self.nodeRoot[funcName]
                fTys = self.funcType (nodeRoot)
                nodeRoot['idx'] = []
                self.symTableF[funcName] = {}
                for fTy, lTy in zip (fTys, nodeRoot['argv']):
                    func = Function (module, fTy, '__' + str (self.idx))
                    self.idx += 1
                    self.symTableF[funcName][str (lTy)] = func
                func = self.symTableF[funcName][str (ty)]
            ctx.value.append (builder.call (func, value))


    # Visit a parse tree produced by ECMAScriptParser#ThisExpression.
    def visitThisExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#FunctionExpression.
    def visitFunctionExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#UnaryMinusExpression.
    def visitUnaryMinusExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#PostDecreaseExpression.
    def visitPostDecreaseExpression(self, ctx):
        single = ctx.singleExpression ()
        ctx.value = []

        single.leaves = ctx.leaves
        self.visit (single)

        for leaf, value in zip (ctx.leaves, single.value):
            val = self.getVal (value, leaf)
            builder = IRBuilder (leaf.getBB ())
            add = builder.add (val, Constant (IntType (32), 1))
            self.setVal (add, value, leaf)
            ctx.value.append (add)


    # Visit a parse tree produced by ECMAScriptParser#AssignmentExpression.
    def visitAssignmentExpression(self, ctx):
        ctx.value = []
        single = ctx.singleExpression ()
        seq = ctx.expressionSequence ()

        seq.leaves = ctx.leaves
        self.visit (seq)
        single.leaves = ctx.leaves
        self.visit (single)

        assignees = single.value
        assigners = seq.value

        for leaf, assignee, assigner in zip (ctx.leaves, assignees, assigners):
            assignee_val = self.getVal (assignee, leaf)
            assigner_val = self.getVal (assigner[0], leaf)
            builder = IRBuilder (leaf.getBB ())

            if type (assignee) is str:
                alloc = builder.alloca (assigner_val.type, name = assignee)
                leaf.setSym (assignee, alloc)
            else:
                if assigner_val.type == assignee_val.type:
                    alloc = assignee
                else:
                    name = str (single.getText ())
                    alloc = builder.alloca (assigner[0].type, name = name)
                    leaf.setSym (name, alloc)
            ctx.value.append (self.setVal (assigner_val, alloc, leaf))

    # Visit a parse tree produced by ECMAScriptParser#TypeofExpression.
    def visitTypeofExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#InstanceofExpression.
    def visitInstanceofExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#UnaryPlusExpression.
    def visitUnaryPlusExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#DeleteExpression.
    def visitDeleteExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#EqualityExpression.
    def visitEqualityExpression(self, ctx):
        comp_op = str (ctx.getChild (1))
        singles = ctx.singleExpression ()
        ctx.value = []

        for single in singles:
            single.leaves = ctx.leaves
            self.visit (single)

        for leaf, lhs, rhs in zip (ctx.leaves, singles[0].value, singles[1].value):
            lhs_val = self.getVal (lhs, leaf)
            rhs_val = self.getVal (rhs, leaf)
            builder = IRBuilder (leaf.getBB ())

            if type (lhs_val) is int:
                lhs_val = Constant (IntType (32), lhs_val)
            elif type (lhs_val) is float:
                lhs_val = Constant (FloatType (), lhs_val)
            if type (rhs_val) is int:
                rhs_val = Constant (IntType (32), lhs_val)
            elif type (rhs_val) is float:
                rhs_val = Constant (FloatType (), rhs_val)

            if type (lhs_val.type) is IntType and type (rhs_val.type) is IntType:
                comp = builder.icmp_signed
            else:
                comp = builder.fcmp_ordered

                if type (lhs_val.type) is IntType:
                    if type (lhs_val) is Constant:
                        lhs_val = Constant (FloatType (), float (singles[0].getText ()))
                    else:
                        lhs_val = builder.sitofp (lhs_val, FloatType ())
                if type (rhs_val.type) is IntType:
                    if type (rhs_val) is Constant:
                        rhs_val = Constant (FloatType (), float (singles[1].getText ()))
                    else:
                        rhs_val = builder.sitofp (rhs_val, FloatType ())
            ctx.value.append (comp (comp_op, lhs_val, rhs_val))


    # Visit a parse tree produced by ECMAScriptParser#BitXOrExpression.
    def visitBitXOrExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#MultiplicativeExpression.
    def visitMultiplicativeExpression(self, ctx):
        ctx.value = []
        singles = ctx.singleExpression ()

        for single in singles:
            single.leaves = ctx.leaves
            self.visit (single)

        for leaf, lhs, rhs in zip (ctx.leaves, singles[0].value, singles[1].value):
            lhs_val = self.getVal (lhs, leaf)
            rhs_val = self.getVal (rhs, leaf)
            builder = IRBuilder (leaf.getBB ())

            if type (lhs_val) is int:
                lhs_val = Constant (IntType (32), lhs_val)
            elif type (lhs_val) is float:
                lhs_val = Constant (FloatType (), lhs_val)
            if type (rhs_val) is int:
                rhs_val = Constant (IntType (32), rhs_val)
            elif type (rhs_val) is float:
                rhs_val = Constant (FloatType (), rhs_val)

            val = None
            if type (lhs_val.type) is IntType and type (rhs_val.type) is IntType:
                mul = builder.mul
                div = builder.sdiv
                rem = builder.srem
            else:
                mul = builder.fmul
                div = builder.fdiv
                rem = builder.frem

                if type (lhs_val.type) is IntType:
                    if type (lhs_val) is Constant:
                        lhs_val = builder.sitofp (lhs_val, FloatType ())
                    else:
                        lhs_val = builder.bitcast (lhs_val, FloatType ())
                elif type (rhs_val.type) is IntType:
                    if type (rhs_val) is Constant:
                        rhs_val = builder.sitofp (rhs_val, FloatType ())
                    else:
                        rhs_val = builder.bitcast (rhs_val, FloatType ())
                else:
                    val = Constant (FloatType (), float ('nan'))

            if val is not None:
                ctx.value.append (val)
            else:
                oper = ctx.getChild (1).getText ()
                if oper == '*':
                    ctx.value.append (mul (lhs_val, rhs_val))
                elif oper == '/':
                    ctx.value.append (div (lhs_val, rhs_val))
                else:
                    ctx.value.append (rem (lhs_val, rhs_val))


    # Visit a parse tree produced by ECMAScriptParser#BitShiftExpression.
    def visitBitShiftExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#ParenthesizedExpression.
    def visitParenthesizedExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#AdditiveExpression.
    def visitAdditiveExpression(self, ctx):
        singles = ctx.singleExpression ()
        ctx.value = []

        for single in singles:
            single.leaves = ctx.leaves
            self.visit (single)

        for leaf, lhs, rhs in zip (ctx.leaves, singles[0].value, singles[1].value):
            lhs_val = self.getVal (lhs, leaf)
            rhs_val = self.getVal (rhs, leaf)
            builder = IRBuilder (leaf.getBB ())

            if type (lhs_val) is int:
                lhs_val = Constant (IntType (32), lhs_val)
            elif type (lhs_val) is float:
                lhs_val = Constant (FloatType (), lhs_val)
            if type (rhs_val) is int:
                rhs_val = Constant (IntType (32), rhs_val)
            elif type (rhs_val) is float:
                rhs_val = Constant (FloatType (), rhs_val)

            if type (lhs_val.type) is IntType and type (rhs_val.type) is IntType:
                add = builder.add
                sub = builder.sub
            else:
                add = builder.fadd
                sub = builder.fsub

                if type (lhs_val.type) is IntType:
                    if type (lhs_val) is Constant:
                        lhs_val = builder.sitofp (lhs_val, FloatType ())
                    else:
                        lhs_val = builder.bitcast (lhs_val, FloatType ())
                if type (rhs_val.type) is IntType:
                    if type (rhs_val) is Constant:
                        rhs_val = builder.sitofp (rhs_val, FloatType ())
                    else:
                        rhs_val = builder.bitcast (rhs_val, FloatType ())

            if ctx.getChild (1).getText () == '+':
                ctx.value.append (add (lhs_val, rhs_val))
            else:
                ctx.value.append (sub (lhs_val, rhs_val))


    # Visit a parse tree produced by ECMAScriptParser#RelationalExpression.
    def visitRelationalExpression(self, ctx):
        comp_op = str (ctx.getChild (1))
        singles = ctx.singleExpression ()
        ctx.value = []

        for single in singles:
            single.leaves = ctx.leaves
            self.visit (single)

        for leaf, lhs, rhs in zip (ctx.leaves, singles[0].value, singles[1].value):
            lhs_val = self.getVal (lhs, leaf)
            rhs_val = self.getVal (rhs, leaf)
            builder = IRBuilder (leaf.getBB ())

            if type (lhs_val) is int:
                lhs_val = Constant (IntType (32), lhs_val)
            elif type (lhs_val) is float:
                lhs_val = Constant (FloatType (), lhs_val)
            if type (rhs_val) is int:
                rhs_val = Constant (IntType (32), rhs_val)
            elif type (rhs_val) is float:
                rhs_val = Constant (FloatType (), rhs_val)

            if type (lhs_val.type) is IntType and type (rhs_val.type) is IntType:
                comp = builder.icmp_signed
            else:
                comp = builder.fcmp_ordered
                if type (lhs_val.type) is IntType:
                    if type (lhs_val) is Constant:
                        lhs_val = Constant (FloatType (), float (singles[0].getText ()))
                    else:
                        lhs_val = builder.bitcast (lhs_val, FloatType ())
                if type (rhs_val.type) is IntType:
                    if type (rhs_val) is Constant:
                        rhs_val = Constant (FloatType (), float (singles[1].getText ()))
                    else:
                        rhs_val = builder.bitcast (rhs_val, FloatType ())

            ctx.value.append (comp (comp_op, lhs_val, rhs_val))


    # Visit a parse tree produced by ECMAScriptParser#PostIncrementExpression.
    def visitPostIncrementExpression(self, ctx):
        single = ctx.singleExpression ()
        ctx.value = []

        single.leaves = ctx.leaves
        self.visit (single)

        for leaf, value in zip (ctx.leaves, single.value):
            val = self.getVal (value, leaf)
            builder = IRBuilder (leaf.getBB ())
            add = builder.add (val, Constant (IntType (32), 1))
            self.setVal (add, value, leaf)
            ctx.value.append (add)


    # Visit a parse tree produced by ECMAScriptParser#BitNotExpression.
    def visitBitNotExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#NewExpression.
    def visitNewExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#LiteralExpression.
    def visitLiteralExpression(self, ctx):
        lit = ctx.literal ()
        lit.leaves = ctx.leaves
        self.visit (lit)
        ctx.value = lit.value


    # Visit a parse tree produced by ECMAScriptParser#ArrayLiteralExpression.
    def visitArrayLiteralExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#MemberDotExpression.
    def visitMemberDotExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#MemberIndexExpression.
    def visitMemberIndexExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#IdentifierExpression.
    def visitIdentifierExpression(self, ctx):
        ctx.value = []
        var = str (ctx.Identifier ())

        for leaf in ctx.leaves:
            ctx.value.append (leaf.getSym (var))


    # Visit a parse tree produced by ECMAScriptParser#BitAndExpression.
    def visitBitAndExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#BitOrExpression.
    def visitBitOrExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#AssignmentOperatorExpression.
    def visitAssignmentOperatorExpression(self, ctx):
        single = ctx.singleExpression ()
        seq = ctx.expressionSequence ()
        operator = str (ctx.getChild (1).getText ())
        ctx.value = []

        seq.leaves = ctx.leaves
        self.visit (seq)
        single.leaves = ctx.leaves
        self.visit (single)

        assignees = single.value
        assigners = seq.value

        for leaf, assignee, assigner in zip (ctx.leaves, assignees, assigners):
            assignee_val = self.getVal (assignee, leaf)
            assigner_val = self.getVal (assigner[0], leaf)
            builder = IRBuilder (leaf.getBB ())


            if assigner_val.type != assignee_val.type:
                if self.asmType (type (assignee_val.type)) > self.asmType (type (assigner_val.type)):
                    assigner_val = builder.sitofp (assigner_val, assignee_val.type)
                else:
                    assignee_val = builder.sitofp (assignee_val, assigner_val.type)
            isInt = assignee_val.type == IntType (32)
            isFloat = assignee_val.type == FloatType ()
            if '+' in operator:
                if isInt:
                    val = builder.add (assignee_val, assigner_val)
                elif isFloat:
                    val = builder.fadd (assignee_val, assigner_val)
            elif '-' in operator:
                if isInt:
                    val = builder.sub (assignee_val, assigner_val)
                elif isFloat:
                    val = builder.fsub (assignee_val, assigner_val)
            elif '*' in operator:
                if isInt:
                    val = builder.mul (assignee_val, assigner_val)
                elif isFloat:
                    val = builder.fmul (assignee_val, assigner_val)
            elif '/' in operator:
                if isInt:
                    val = builder.sdiv (assignee_val, assigner_val)
                elif isFloat:
                    val = builder.fdiv (assignee_val, assigner_val)
            elif '%' in operator:
                if isInt:
                    val = builder.srem (assignee_val, assigner_val)
                elif isFloat:
                    val = builder.frem (assignee_val, assigner_val)
            elif '|' in operator:
                if isInt:
                    assignee_val = builder.bitcast (assignee_val, IntType (32))
                elif isFloat:
                    assigner_val = builder.bitcast (assigner_val, IntType (32))
                val = builder.or_ (assignee_val, assigner_val)
            elif '&' in operator:
                if isInt:
                    assignee_val = builder.bitcast (assignee_val, IntType (32))
                elif isFloat:
                    assigner_val = builder.bitcast (assigner_val, IntType (32))
                val = builder.and_ (assignee_val, assigner_val)
            elif '>>>' in operator:
                if isInt:
                    assignee_val = builder.bitcast (assignee_val, IntType (32))
                elif isFloat:
                    assigner_val = builder.bitcast (assigner_val, IntType (32))
                val = builder.lshr (assignee_val, assigner_val)
            elif '>>' in operator:
                if isInt:
                    assignee_val = builder.bitcast (assignee_val, IntType (32))
                elif isFloat:
                    assigner_val = builder.bitcast (assigner_val, IntType (32))
                val = builder.ashr (assignee_val, assigner_val)
            elif '<<' in operator:
                if isInt:
                    assignee_val = builder.bitcast (assignee_val, IntType (32))
                elif isFloat:
                    assigner_val = builder.bitcast (assigner_val, IntType (32))
                val = builder.shl (assignee_val, assigner_val)

            store = self.setVal (val, assignee, leaf)
            ctx.value.append (assignee)


    # Visit a parse tree produced by ECMAScriptParser#VoidExpression.
    def visitVoidExpression(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#assignmentOperator.
    def visitAssignmentOperator(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#literal.
    def visitLiteral(self, ctx):
        numeric = ctx.numericLiteral ()

        if numeric is not None:
            numeric.leaves = ctx.leaves
            self.visit (numeric)
            ctx.value = ctx.numericLiteral ().value
        else:
            ctx.value = Constant.string (str (ctx.StringLiteral ()))


    # Visit a parse tree produced by ECMAScriptParser#numericLiteral.
    def visitNumericLiteral(self, ctx):
        num = str (ctx.getText ())

        ctx.value = []

        for leaf in ctx.leaves:
            if '.' in num:
                ctx.value.append (Constant (FloatType (), num))
            else:
                ctx.value.append (Constant (IntType (32), num))


    # Visit a parse tree produced by ECMAScriptParser#identifierName.
    def visitIdentifierName(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#reservedWord.
    def visitReservedWord(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#keyword.
    def visitKeyword(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#futureReservedWord.
    def visitFutureReservedWord(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#getter.
    def visitGetter(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#setter.
    def visitSetter(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#eos.
    def visitEos(self, ctx):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ECMAScriptParser#eof.
    def visitEof(self, ctx):
        return self.visitChildren(ctx)


