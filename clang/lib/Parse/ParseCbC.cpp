#ifndef noCbC

#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/PrettyStackTrace.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/AST/PrettyDeclStackTrace.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/Lookup.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/Sema/SemaDiagnostic.h"
#include "clang/Sema/ScopeInfo.h"

#include <sstream>
#include <string>
#include "CbCHelper.h"

using namespace clang;

/// The class which belong to this namespace is from other files' namespace.
/// Because those namespaces are unnamed namespaces, we can't access them.
/// So create this namespace and copy classes from those namespaces.
namespace ExternalSpace { // from ParseExpr.cpp , ParseStmt.cpp
    class CastExpressionIdValidator final : public CorrectionCandidateCallback {
     public:
      CastExpressionIdValidator(Token Next, bool AllowTypes, bool AllowNonTypes)
          : NextToken(Next), AllowNonTypes(AllowNonTypes) {
        WantTypeSpecifiers = WantFunctionLikeCasts = AllowTypes;
      }

      bool ValidateCandidate(const TypoCorrection &candidate) override {
        NamedDecl *ND = candidate.getCorrectionDecl();
        if (!ND)
          return candidate.isKeyword();

        if (isa<TypeDecl>(ND))
          return WantTypeSpecifiers;

        if (!AllowNonTypes || !CorrectionCandidateCallback::ValidateCandidate(candidate))
          return false;

        if (!NextToken.isOneOf(tok::equal, tok::arrow, tok::period))
          return true;

        for (auto *C : candidate) {
          NamedDecl *ND = C->getUnderlyingDecl();
          if (isa<ValueDecl>(ND) && !isa<FunctionDecl>(ND))
            return true;
        }
        return false;
      }

      std::unique_ptr<CorrectionCandidateCallback> clone() override {
        return std::make_unique<CastExpressionIdValidator>(*this);
      }

     private:
      Token NextToken;
      bool AllowNonTypes;
    };

    class StatementFilterCCC final : public CorrectionCandidateCallback {
    public:
      StatementFilterCCC(Token nextTok) : NextToken(nextTok) {
        WantTypeSpecifiers = nextTok.isOneOf(tok::l_paren, tok::less, tok::l_square,
                                             tok::identifier, tok::star, tok::amp);
        WantExpressionKeywords =
            nextTok.isOneOf(tok::l_paren, tok::identifier, tok::arrow, tok::period);
        WantRemainingKeywords =
            nextTok.isOneOf(tok::l_paren, tok::semi, tok::identifier, tok::l_brace);
        WantCXXNamedCasts = false;
      }

      bool ValidateCandidate(const TypoCorrection &candidate) override {
        if (FieldDecl *FD = candidate.getCorrectionDeclAs<FieldDecl>())
          return !candidate.getCorrectionSpecifier() || isa<ObjCIvarDecl>(FD);
        if (NextToken.is(tok::equal))
          return candidate.getCorrectionDeclAs<VarDecl>();
        if (NextToken.is(tok::period) &&
            candidate.getCorrectionDeclAs<NamespaceDecl>())
          return false;
        return CorrectionCandidateCallback::ValidateCandidate(candidate);
      }

      std::unique_ptr<CorrectionCandidateCallback> clone() override {
        return std::make_unique<StatementFilterCCC>(*this);
      }

    private:
      Token NextToken;
    };
}

/// Prepare__retForGotoWithTheEnvExpr - Prepare __CbC_return, code segment for returning and some necessary statements.
/// It is called when the parser find __return and statements are put into complex statement.
/// 
/// examples which are created:
///   complex statement:
///         ({
///           __code (*__CbC_return)(return_type, void*);
///           __CbC_return = code_segment_for_return;
///           __CbC_return;
///         });

ExprResult Parser::Prepare__retForGotoWithTheEnvExpr(){

  if (isVoidFunction()) { // error check : function type is void or not.
    unsigned DiagID = Diags.getCustomDiagID(DiagnosticsEngine::Error, "continuation with the environment cannot use in the void function");
    Diag(Tok, DiagID);
    return ExprError();
  }
  // std::unique_ptr<ASTUnit> AST(tooling::buildASTFromCode("({auto x = 1 + 1;})"));

  StmtResult innerRes;
  SourceLocation Loc = Tok.getLocation();
  IdentifierInfo *__CbC_retII = CreateIdentifierInfo("__CbC_cont", Loc);
  IdentifierInfo *retcsII = CreateUniqueIdentifierInfo(__CBC_RET_CODE_BASE_NAME, Loc);
  Create__CbC_envStruct(Loc, AS_none);

  Actions.ActOnStartStmtExpr();
  StmtResult CompoundStmtRes;
  PrettyStackTraceLoc CrashInfo(PP.getSourceManager(),Loc,"in compound statement ('{}')");
  StmtVector CompoundStmts; 

  ConsumeAnyToken(); // eat the '__return'.
  // create code segment for return to C's function
  CreateRetCS(retcsII);
    
  // __code (*__CbC_return)();
  innerRes = CreateDeclStmt(__CbC_retII, true, false, 0, DeclSpec::TST___code);
  if (innerRes.isUsable())
    CompoundStmts.push_back(innerRes.get());

  // __CbC_return = ret;
  innerRes = CreateAssignmentStmt(__CbC_retII, retcsII);
  if (innerRes.isUsable())
    CompoundStmts.push_back(innerRes.get());

  // __CbC_return;
  innerRes = CreateComplexStmtRet(__CbC_retII, false);
  if (innerRes.isUsable())
    CompoundStmts.push_back(innerRes.get());
  Sema::CompoundScopeRAII CompoundScope(Actions);
  CompoundStmtRes = Actions.ActOnCompoundStmt(Loc,Loc,CompoundStmts,true);

  return Actions.ActOnStmtExpr(getCurScope(),Loc, CompoundStmtRes.get(), Loc);
}

#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/Token.h"

bool Parser::isBuiltinSetjmpDefined() {
  IdentifierInfo *II = PP.getIdentifierInfo("__builtin_setjmp");
  if (II != nullptr) {
    int BuiltinID = II->getBuiltinID();
    return (BuiltinID == Builtin::BI__builtin_setjmp);
  }
  return false;
}

/// Prepare__envForGotoWithTheEnvExpr - Prepare __CbC_environment, struct __CbC_env and some necessary statements.
/// It is called when the parser find __environment and statements are put into complex statement.
/// 
/// examples which are created:
///   complex statement:
///         ({
///           volatile struct __CbC_env __CbC_environment;
///           jmp_buf env_buf;  --> int env_buf[64];
///           extern int setjmp(jmp_buf);
///           return_type retval;
///           __CbC_environment.ret_p = &retval;
///           __CbC_environment.env = &env_buf;
///           if (setjmp(__CbC_environment.env)){
///             return retval;
///           }
///           &__CbC_environment;
///         });
///   struct __CbC_env:
///         struct __CbC_env{
///           void *ret_p,*env;
///         }

//
// Compile from String
//   make sure to CosumeToken() before call this function.
//
void Parser::CompileFromString(const char *str, StmtVector &CompoundStmts){
  SourceLocation Loc = Tok.getLocation();
  Token TokSave = Tok;
  SourceLocation PLocSave = PrevTokLocation;
  PP.ReadFromString(str,Loc); 
  ConsumeAnyToken();
  while(! Tok.is(tok::eof)) {
      StmtVector Stmts;
      StmtResult innerRes;
      innerRes = ParseStatementOrDeclaration(Stmts,( ParsedStmtContext::Compound | ParsedStmtContext::AllowDeclarationsInC));
      if (innerRes.isUsable())
        CompoundStmts.push_back(innerRes.get());
  }
  Tok = TokSave ;
  PrevTokLocation= PLocSave ;
}

ExprResult Parser::Prepare__envForGotoWithTheEnvExpr(){

  if (isVoidFunction()) { // error check : function type is void or not.
    unsigned DiagID = Diags.getCustomDiagID(DiagnosticsEngine::Error, "continuation with the environment cannot use in the void function");
    Diag(Tok, DiagID);
    return ExprError();
  }

  Sema::CompoundScopeRAII CompoundScope(Actions);
  StmtResult innerRes;
  SourceLocation Loc = Tok.getLocation();
  IdentifierInfo *bufII = CreateIdentifierInfo(__CBC_BUF_NAME, Loc);
  IdentifierInfo *retvalII = CreateIdentifierInfo(__CBC_RETVAL_NAME, Loc);
  IdentifierInfo *structII = CreateIdentifierInfo(__CBC_STRUCT_NAME, Loc);
  IdentifierInfo *__CbC_envII = CreateIdentifierInfo(__CBC_ENVIRONMENT_NAME, Loc);
  IdentifierInfo *envII = CreateIdentifierInfo(__CBC_STRUCT_ENV_NAME, Loc);
  IdentifierInfo *ret_pII = CreateIdentifierInfo(__CBC_STRUCT_POINTER_NAME, Loc);
  Create__CbC_envStruct(Loc, AS_none);
  Actions.ActOnStartStmtExpr();

  PrettyStackTraceLoc CrashInfo(PP.getSourceManager(),Loc,"in compound statement ('{}')");
  StmtVector CompoundStmts; 
  ExprResult Result(true);
  
 // struct __CbC_env __CbC_environment;
  innerRes = CreateDeclStmt(__CbC_envII, false, false, 0, DeclSpec::TST_struct, structII, DeclSpec::TQ_volatile);
  if (innerRes.isUsable())
    CompoundStmts.push_back(innerRes.get());

  // returnType retval;
  innerRes = CreateDeclStmt(retvalII, false, true);
  if (innerRes.isUsable())
    CompoundStmts.push_back(innerRes.get());

  ConsumeAnyToken(); // eat the '__environment'.
  if (isBuiltinSetjmpDefined()) {
      CompileFromString("int env_buf[64];",CompoundStmts); // 4*64 is enough for every arch right now
  } else {
      CompileFromString("int env_buf[64];extern int setjmp(void *);",CompoundStmts); 
  }

  // __CbC_environment.ret_p = &retval;
  innerRes = CreateAssignmentStmt(__CbC_envII, retvalII, true, true, ret_pII);
  if (innerRes.isUsable())
    CompoundStmts.push_back(innerRes.get());

  // __CbC_environment.env = env_buf;
  innerRes = CreateAssignmentStmt(__CbC_envII, bufII, true, false, envII);
  if (innerRes.isUsable())
    CompoundStmts.push_back(innerRes.get());

  // create statements of setjmp
  innerRes = CreateSjForContinuationWithTheEnv();
  if (innerRes.isUsable())
    CompoundStmts.push_back(innerRes.get());
  
  // __CbC_environment;
  innerRes = CreateComplexStmtRet(__CbC_envII, true);
  if (innerRes.isUsable())
    CompoundStmts.push_back(innerRes.get());

  StmtResult CompoundStmtRes = Actions.ActOnCompoundStmt(Loc,Loc,CompoundStmts,true);
  Result =  Actions.ActOnStmtExpr(getCurScope() ,Loc, CompoundStmtRes.get(), Loc);

  // cast 
  ParsedType CastTy;
  DeclSpec void_DS(AttrFactory);
  setTST(&void_DS, DeclSpec::TST_void);
  Declarator DeclaratorInfo(void_DS, ParsedAttributesView::none(), DeclaratorContext::TypeName);
  DeclSpec star_DS(AttrFactory);
  star_DS.Finish(Actions, Actions.getASTContext().getPrintingPolicy());
  DeclaratorInfo.ExtendWithDeclSpec(star_DS);
  DeclaratorInfo.SetIdentifier(nullptr, Tok.getLocation());
  DeclaratorInfo.AddTypeInfo(DeclaratorChunk::getPointer(star_DS.getTypeQualifiers(), Loc,star_DS.getConstSpecLoc(),star_DS.getVolatileSpecLoc(),
                                                         star_DS.getRestrictSpecLoc(),star_DS.getAtomicSpecLoc(),star_DS.getUnalignedSpecLoc()),std::move(star_DS.getAttributes()),SourceLocation());
  return Actions.ActOnCastExpr(getCurScope(), Loc, DeclaratorInfo, CastTy,Loc, Result.get());
  
}

/// CreateAssignmentStmt - Create assignment statement such as "aaa = bbb;", "auaua = llll;", etc.
/// It can create 4 kinds of statement.
/// 1. common assignment statement:
///         variable '=' variable ';'
/// 2. LHS variable is member of struct:
///         structVar '.' member '=' variable ';'
/// 3. RHS variable is address of operand:
///         variable '=' '&' variable ';'
/// 4. 2+3:
///         structVar '.' member '=' '&' variable ';'
StmtResult Parser::CreateAssignmentStmt(IdentifierInfo* LHSII, IdentifierInfo* RHSII, bool LHSisMemberAccess, bool RHShasAmp,
                                        IdentifierInfo* extraLHSII, IdentifierInfo* extraRHSII){
  ExprResult Expr,LHS,RHS;
  
  Token Next,LHSToken;
  SourceLocation Loc = Tok.getLocation();
  CXXScopeSpec SS;
  Next.startToken();
  if (LHSisMemberAccess) 
     Next.setKind(tok::period);
  else
     Next.setKind(tok::equal);
  LHSToken.startToken();
  LHSToken.setLocation(Loc);
  LHSToken.setIdentifierInfo(LHSII);
  LHSToken.setKind(tok::identifier);
  ExternalSpace::StatementFilterCCC SFCCC(Next);
  Sema::NameClassification Classification = Actions.ClassifyName(getCurScope(), SS, LHSII, Loc, Next, SS.isEmpty() ? &SFCCC : nullptr);
  if (Classification.getKind() == Sema::NC_NonType) {
     Token Tok;
     Tok.startToken();
     Tok.setKind(tok::annot_non_type);
     setNonTypeAnnotation(Tok, Classification.getNonTypeDecl());
     Tok.setLocation(Loc);
     Tok.setAnnotationEndLoc(Loc);
     NamedDecl *ND = getNonTypeAnnotation(Tok);
     LHS = Actions.ActOnNameClassifiedAsNonType(getCurScope(), SS, ND, Loc, Tok);
  } else { 
     setExprAnnotation(LHSToken, Classification.getExpression());
     LHSToken.setAnnotationEndLoc(Loc);
     PP.AnnotateCachedTokens(LHSToken);
     LHS = getExprAnnotation(LHSToken);
  }
  if (LHSisMemberAccess) 
    LHS = LookupMemberAndBuildExpr(extraLHSII, LHS.get(), false);
  
  RHS = LookupNameAndBuildExpr(RHSII);
  if (RHShasAmp)
    RHS = Actions.ActOnUnaryOp(getCurScope(), Loc, tok::amp, RHS.get());

  Expr = Actions.ActOnBinOp(getCurScope(), Loc,tok::equal,LHS.get(),RHS.get());
  
  return Actions.ActOnExprStmt(Expr);
}

/// CreateDeclStmt - Create declaration statement such as "int aaa;".
/// If isRetCS is true, create code segment for return to C's function. And Name is name of code segment.
/// If copyType is true, type of variable is copied from callee.
StmtResult Parser::CreateDeclStmt(IdentifierInfo *II, bool isRetCS, bool copyType, int array, DeclSpec::TST valueType, IdentifierInfo* Name, DeclSpec::TQ TQ){
  const PrintingPolicy &Policy = Actions.getASTContext().getPrintingPolicy();
  SourceLocation Loc = Tok.getLocation();
  DeclGroupPtrTy DeclGPT;
  ParsingDeclSpec DS(*this);
  DeclSpec *DSp;
  DSp = &DS;

  setTST(&DS, valueType, Name, TQ);
  ParsedAttributes LocalAttrs(AttrFactory);
  // LocalAttrs.takeAllFrom(Attrs);
  ParsingDeclarator D(*this, DS, LocalAttrs, static_cast<DeclaratorContext>(DeclaratorContext::Block));
  D.SetIdentifier(II, Loc);
    
  if (array) {
    DeclSpec FDS(AttrFactory);
    D.AddTypeInfo(DeclaratorChunk::getArray(0,false,false,Actions.ActOnIntegerConstant(Loc,array).get(),Loc, FDS.getConstSpecLoc() ),
            std::move(FDS.getAttributes()), SourceLocation());
  }
  if (isRetCS) {
    D.setEllipsisLoc(SourceLocation());
    bool hadGroupingParens = D.hasGroupingParens();
    D.setGroupingParens(true);
    D.SetRangeEnd(Loc);
    DeclSpec FDS(AttrFactory);
    DS.Finish(Actions, Policy);
    
    D.AddTypeInfo(DeclaratorChunk::getPointer(FDS.getTypeQualifiers(), Loc, FDS.getConstSpecLoc(), FDS.getVolatileSpecLoc(),
                                              FDS.getRestrictSpecLoc(), DS.getAtomicSpecLoc(), FDS.getUnalignedSpecLoc()), std::move(FDS.getAttributes()), SourceLocation());
    D.setGroupingParens(hadGroupingParens);
    
    
    ParseScope PrototypeScope(this,Scope::FunctionPrototypeScope|Scope::DeclScope|
                              (D.isFunctionDeclaratorAFunctionDeclaration() ? Scope::FunctionDeclarationScope : 0));
    bool HasProto = false;
    SmallVector<DeclaratorChunk::ParamInfo, 16> ParamInfo;
    SourceLocation EllipsisLoc, RefQualifierLoc; 
    DeclSpec FPDS(AttrFactory);
    bool RefQualifierIsLValueRef = true;
    ExceptionSpecificationType ESpecType = EST_None;
    SourceRange ESpecRange;
    SmallVector<ParsedType, 2> DynamicExceptions;
    SmallVector<SourceRange, 2> DynamicExceptionRanges;
    ExprResult NoexceptExpr;
    CachedTokens *ExceptionSpecTokens = 0;
    ParsedAttributes FnAttrs(AttrFactory);
    TypeResult TrailingReturnType;

    ParmVarDecl *Param;
    FunctionDecl *CurFunctionDecl = Actions.getCurFunctionDecl();
    QualType CurFuncResQT = CurFunctionDecl->getReturnType();
    TypeSourceInfo *CurFuncTI = Actions.Context.CreateTypeSourceInfo(CurFuncResQT);
    
    Param = CreateParam();
    Param->setTypeSourceInfo(CurFuncTI);
    Param->setType(CurFuncResQT);
    ParamInfo.push_back(DeclaratorChunk::ParamInfo(0, Loc, Param, 0));
    Param = CreateParam(0, 1, DeclSpec::TST_void);
    ParamInfo.push_back(DeclaratorChunk::ParamInfo(0, Loc, Param, 0));
    HasProto = true;
    
    D.AddTypeInfo(DeclaratorChunk::getFunction(HasProto, ESpecType, Loc, ParamInfo.data(),
                                               ParamInfo.size(), EllipsisLoc, Loc, 
                                               RefQualifierIsLValueRef, RefQualifierLoc,
                                               /*MutableLoc=*/SourceLocation(),
                                               ESpecType, ESpecRange.getBegin(),
                                               DynamicExceptions.data(), DynamicExceptionRanges.data(),
                                               DynamicExceptions.size(),
                                               // DynamicExceptions,
                                               NoexceptExpr.isUsable() ? NoexceptExpr.get() : 0, ExceptionSpecTokens, None, Loc, Loc, D, TrailingReturnType,Loc, &FPDS),
                  std::move(FnAttrs), Loc);
    PrototypeScope.Exit();
    DSp = &FDS;
  }
  
  SmallVector<Decl *, 8> DeclsInGroup;
  Decl *FirstDecl;
  
  if (copyType)
    FirstDecl = HandleDeclAndChangeDeclType(D);
  else
    FirstDecl = ParseDeclarationAfterDeclaratorAndAttributes(D);
  
  D.complete(FirstDecl);
  DeclsInGroup.push_back(FirstDecl);
  DeclGPT =  Actions.FinalizeDeclaratorGroup(getCurScope(), *DSp, DeclsInGroup);
  return Actions.ActOnDeclStmt(DeclGPT, Loc, Loc);
}


/// handleDeclAndChangeDeclType - This function imitated Parser::ParseDeclarationAfterDeclaratorAndAttributes() and Sema::ActOnDeclarator().
/// The origins get Type from Declarator but this function get Type from current function.
/// It is useful for CbC to create statements for the continuation with the environments.
Decl* Parser::HandleDeclAndChangeDeclType(Declarator &D) {
  D.setFunctionDefinitionKind(FunctionDefinitionKind::Declaration);
  DeclarationNameInfo NameInfo = Actions.GetNameForDeclarator(D);
  DeclContext *DC = Actions.CurContext;
  QualType R = Actions.getCurFunctionDecl()->getReturnType(); // copy a type
  TypeSourceInfo *TInfo = Actions.Context.CreateTypeSourceInfo(R); // copy a type infomation
  Scope *S = getCurScope();
  LookupResult Previous(Actions, NameInfo, Actions.LookupOrdinaryName, Actions.ForVisibleRedeclaration);
  bool IsLinkageLookup = false;
  bool CreateBuiltins = false;
  
  // If the declaration we're planning to build will be a function
  // or object with linkage, then look for another declaration with
  // linkage (C99 6.2.2p4-5 and C++ [basic.link]p6).
  //
  // If the declaration we're planning to build will be declared with
  // external linkage in the translation unit, create any builtin with
  // the same name.
  if (R->isFunctionType()) {
    IsLinkageLookup = true;
    CreateBuiltins =
      Actions.CurContext->getEnclosingNamespaceContext()->isTranslationUnit();
  } else if (Actions.CurContext->getRedeclContext()->isTranslationUnit())
    CreateBuiltins = true;
  
  if (IsLinkageLookup)
    Previous.clear(Actions.LookupRedeclarationWithLinkage);
  
  Actions.LookupName(Previous, S, CreateBuiltins);

  // In C++, the previous declaration we find might be a tag type
  // (class or enum). In this case, the new declaration will hide the
  // tag type. Note that this does does not apply if we're declaring a
  // typedef (C++ [dcl.typedef]p4).
  if (Previous.isSingleTagDecl())
    Previous.clear();
  NamedDecl *New;
  bool AddToScope = true;
  if (R->isFunctionType()) {
    New = Actions.ActOnFunctionDeclarator(S, D, DC, TInfo, Previous,
                                          MultiTemplateParamsArg(), AddToScope);
  } else {
    New = Actions.ActOnVariableDeclarator(S, D, DC, TInfo, Previous,
                                          MultiTemplateParamsArg(), AddToScope);
  }
  
  if (New->getDeclName() && AddToScope) {
    // Only make a locally-scoped extern declaration visible if it is the first
    // declaration of this entity. Qualified lookup for such an entity should
    // only find this declaration if there is no visible declaration of it.
    bool AddToContext = !D.isRedeclaration() || !New->isLocalExternDecl();
    Actions.PushOnScopeChains(New, S, AddToContext);
    if (!AddToContext)
      Actions.CurContext->addHiddenDecl(New);
  }
  
  return New;
}

/// CreateSjForContinuationWithEnv - Create statements of setjmp for continuation with the environment.
///   code example:
///         if (setjmp(__CbC_environment.env)){
///           return retval;
///         }
StmtResult Parser::CreateSjForContinuationWithTheEnv(){
  SourceLocation Loc = Tok.getLocation();
  StmtResult IfRes;
  ParseScope IfScope(this, Scope::DeclScope | Scope::ControlScope, true/* C99 or CXX */);
  ExprResult CondExp;
  StmtResult InitStmt;
  Sema::ConditionResult Cond;

  if (isBuiltinSetjmpDefined()) {
      CondExp = LookupNameAndBuildExpr(CreateIdentifierInfo("__builtin_setjmp", Loc));
  } else {
      CondExp = LookupNameAndBuildExpr(CreateIdentifierInfo("setjmp", Loc));
  }
  ExprVector ArgExprs;
  ExprResult __envExprRes = CondExp.get();

  __envExprRes = LookupNameAndBuildExpr(CreateIdentifierInfo(__CBC_ENVIRONMENT_NAME, Loc));
  __envExprRes = LookupMemberAndBuildExpr(CreateIdentifierInfo(__CBC_STRUCT_ENV_NAME, Loc), __envExprRes.get(), false);

  ArgExprs.push_back(__envExprRes.get());
  CondExp = Actions.ActOnCallExpr(getCurScope(), CondExp.get(), Loc, ArgExprs, Loc, 0);
  Cond    = Actions.ActOnCondition(getCurScope(), Loc, CondExp.get(), Sema::ConditionKind::Boolean);


  ParseScope InnerScope(this, Scope::DeclScope,false);
    
  StmtResult StmtRes;
  Sema::CompoundScopeRAII CompoundScope(Actions);
  PrettyStackTraceLoc CrashInfo(PP.getSourceManager(),Loc,"in create setjmp statement for CbC");
  StmtVector innerStmts;
  StmtResult innerStmtRes;
  ExprResult innerExprRes;
  innerExprRes = LookupNameAndBuildExpr(CreateIdentifierInfo(__CBC_RETVAL_NAME, Loc));
  innerStmtRes = Actions.ActOnReturnStmt(Loc, innerExprRes.get(), getCurScope());
  if (innerStmtRes.isUsable())
    innerStmts.push_back(innerStmtRes.get());
  StmtRes = Actions.ActOnCompoundStmt(Loc, Loc,innerStmts, false);
  StmtResult ThenStmt(StmtRes);
  InnerScope.Exit();
  IfScope.Exit();
  StmtResult ElseStmt;
  IfRes = Actions.ActOnIfStmt(Loc, IfStatementKind::Ordinary, Loc, CondExp.get(), Cond, Loc, ThenStmt.get(),Loc, ElseStmt.get());
  return IfRes;
}


/// LookupNameAndBuildExpr - Look up name, create ExprResult and return it.
ExprResult Parser::LookupNameAndBuildExpr(IdentifierInfo *II, bool IsAddressOfOperand){
  SourceLocation Loc = Tok.getLocation();
  UnqualifiedId Name;
  CXXScopeSpec SS;
  SourceLocation TemplateKWLoc;
  ExternalSpace::CastExpressionIdValidator Validator(Tok,false,true);
  Name.setIdentifier(II, Loc);
  return Actions.ActOnIdExpression(getCurScope(), SS, TemplateKWLoc, Name, false, IsAddressOfOperand, &Validator);
}

/// LookupMemberAndBuildExpr - Look up member name, create ExprResult and return it.
/// If IsArrow is true, the name is accessed by arrow operand.
ExprResult Parser::LookupMemberAndBuildExpr(IdentifierInfo *II, Expr* Base, bool IsArrow){
  SourceLocation Loc = Tok.getLocation();
  CXXScopeSpec SS;
  UnqualifiedId Name;
  SourceLocation TemplateKWLoc;
  tok::TokenKind OpKind = (IsArrow ? tok::arrow : tok::period);
  Name.setIdentifier(II,Loc);
  return Actions.ActOnMemberAccessExpr(getCurScope(), Base, Loc, OpKind, SS, TemplateKWLoc, Name, nullptr);
}


/// Create__CbC_envStruct - This method create "struct __CbC_env" which is used to continuation with environment.
/// If the __CbC_env has been already defined, it doesn't create __CbC_env again.
///   The example of struct which is created :
///        struct __CbC_env{
///          void *ret_p,*env;
///        };
void Parser::Create__CbC_envStruct(SourceLocation Loc, AccessSpecifier AS) {

  IdentifierInfo *Name = CreateIdentifierInfo(__CBC_STRUCT_NAME, Loc);
  // Check previous definition. If the __CbC_env has been already defined, we have not to create again.
  LookupResult Previous(Actions, Name, Loc, Actions.LookupTagName, Actions.ForVisibleRedeclaration);
  if(Actions.LookupName(Previous, getCurScope()))
    return;

  Scope *SavedScope = getCurScope();
  DeclContext *SavedContext = Actions.CurContext;
  sema::FunctionScopeInfo *SavedFSI = Actions.FunctionScopes.pop_back_val();

  Actions.CurContext = static_cast<DeclContext *>(Actions.Context.getTranslationUnitDecl());
  Scope *TopScope = getCurScope();
  while(TopScope->getParent() != NULL)
    TopScope = TopScope->getParent();
  Actions.CurScope = TopScope;

  ParsingDeclSpec SDS(*this);
  SDS.SetRangeStart(Loc);
  SDS.SetRangeEnd(Loc);
  DeclSpec::TST TagType = DeclSpec::TST_struct;
  DeclResult TagOrTempResult = true;
  bool Owned = false;
  bool IsDependent = false;
  ParsedAttributes attrs(AttrFactory);
  MultiTemplateParamsArg TParams;
      
  TagOrTempResult = Actions.ActOnTag(getCurScope(), TagType, Sema::TUK_Definition, Loc,
                                     SDS.getTypeSpecScope(), Name, Loc, attrs, AS,
                                     SDS.getModulePrivateSpecLoc(), TParams, Owned, IsDependent,
                                     SourceLocation(), false, clang::TypeResult(), false, false);

  Decl *TagDecl = TagOrTempResult.get();
  PrettyDeclStackTraceEntry CrashInfo(Actions.Context, TagDecl, Loc, "parsing struct/union body");
  ParseScope StructScope(this, Scope::ClassScope|Scope::DeclScope);
  Actions.ActOnTagStartDefinition(getCurScope(), TagDecl);
  SmallVector<Decl *, 32> FieldDecls;

  FieldDecls.push_back(Create__CbC_envBody(TagDecl, DeclSpec::TST_void, Loc, __CBC_STRUCT_POINTER_NAME));
  FieldDecls.push_back(Create__CbC_envBody(TagDecl, DeclSpec::TST_void, Loc, __CBC_STRUCT_ENV_NAME));

  Actions.ActOnFields(getCurScope(),Loc, TagDecl, FieldDecls,Loc, Loc,attrs);
  StructScope.Exit();
  Actions.ActOnTagFinishDefinition(getCurScope(), TagDecl, Loc);

  Actions.CurScope = SavedScope;
  Actions.CurContext = SavedContext;
  Actions.FunctionScopes.push_back(SavedFSI);
}

/// Create__CbC_envBody - Create void type pointer ret_p and env which are member of __CbC_env.
Decl* Parser::Create__CbC_envBody(Decl* TagDecl, DeclSpec::TST T, SourceLocation Loc, const char* Name){
  const PrintingPolicy &Policy = Actions.getASTContext().getPrintingPolicy();
  ParsingDeclSpec PDS(*this);
  setTST(&PDS, T);
  SourceLocation CommaLoc;
  ParsedAttributes LocalAttrs(AttrFactory);
  // LocalAttrs.takeAllFrom(Attrs);
  ParsingFieldDeclarator DeclaratorInfo(*this, PDS, LocalAttrs);
  DeclaratorInfo.D.setCommaLoc(CommaLoc);
  DeclaratorInfo.D.SetRangeEnd(Loc);
  DeclSpec DS(AttrFactory);
  DS.Finish(Actions, Policy);
  DeclaratorInfo.D.SetIdentifier(CreateIdentifierInfo(Name, Loc),Loc);

  DeclaratorInfo.D.AddTypeInfo(DeclaratorChunk::getPointer(DS.getTypeQualifiers(), Loc,DS.getConstSpecLoc(),
                                                           DS.getVolatileSpecLoc(),DS.getRestrictSpecLoc(), DS.getAtomicSpecLoc(), DS.getUnalignedSpecLoc()),
                               std::move(DS.getAttributes()),SourceLocation());
  Decl *Field = Actions.ActOnField(getCurScope(), TagDecl,
                                   DeclaratorInfo.D.getDeclSpec().getSourceRange().getBegin(),
                                   DeclaratorInfo.D, DeclaratorInfo.BitfieldSize);
  DeclaratorInfo.complete(Field);
  return Field;
}

/// CreateIdentifierInfo - Create IdentifierInfo from char pointer.
///   usage : 
///          IdentifierInfo *II = CreateIdentifierInfo(IIName, Kind, Location);

IdentifierInfo* Parser::CreateIdentifierInfo(const char* Name, SourceLocation Loc) {
  int length = strlen(Name);
  Token TokenForII;
  TokenForII.startToken();
  TokenForII.setLocation(Loc);
  TokenForII.setLength(length);
  TokenForII.setKind(tok::raw_identifier);
  TokenForII.setRawIdentifierData(Name);
  IdentifierInfo *II;
  II = PP.getIdentifierInfo(TokenForII.getRawIdentifier());
  TokenForII.setIdentifierInfo(II);
  TokenForII.setKind(II->getTokenID());
  return II;
}

/// CreateUniqueIdentifierInfo - Create unique IdentifierInfo.
/// IdentifierInfos have unique name which were created by this function.
/// Naming conventions : 
///   current 'function name' '..' 'variable name' 'uniqueID'
/// For example, if current function's name is 'main' and variable name is 'auaua', IdentifierInfo's name is 'main..auaua'.
IdentifierInfo* Parser::CreateUniqueIdentifierInfo(const char* Name, SourceLocation Loc){
  IdentifierInfo *II;
  std::ostringstream os;
  
  os << curFuncName << ".." /* separator */ << Name << UniqueId;
  II = CreateIdentifierInfo(os.str().c_str(), Loc);
  UniqueId++; // Modify the unique ID.
  return II;
}

/// CreateRetCS - Create code segment which is used for continuation with the environment.
///   create these codes:
///         __code ret(return_type retval, void *env){
///           *(return_type)((struct CbC_environment *)(env))->ret_p = retval;
///             longjmp((void*)(((struct __CbC_environment *)env)->env),1);
///         }
void Parser::CreateRetCS(IdentifierInfo *csName){
  const PrintingPolicy &Policy = Actions.getASTContext().getPrintingPolicy();
  QualType CurFuncResQT = Actions.getCurFunctionDecl()->getReturnType();
  
  Scope *SavedScope = getCurScope();
  DeclContext *SavedContext = Actions.CurContext;
  TypeSourceInfo *CurFuncTI = Actions.Context.CreateTypeSourceInfo(CurFuncResQT);
  sema::FunctionScopeInfo *SavedFSI = Actions.FunctionScopes.pop_back_val();

  Actions.CurContext = static_cast<DeclContext *>(Actions.Context.getTranslationUnitDecl());
  Scope *TopScope = getCurScope();
  while(TopScope->getParent() != NULL)
    TopScope = TopScope->getParent();
  Actions.CurScope = TopScope;

  DeclGroupPtrTy returnDecl = DeclGroupPtrTy();
  SourceLocation Loc = Tok.getLocation();
  ParsingDeclSpec PDS(*this);
  setTST(&PDS, DeclSpec::TST___code);
  ParsedAttributes LocalAttrs(AttrFactory);
  // LocalAttrs.takeAllFrom(Attrs);
  ParsingDeclarator D(*this, PDS, LocalAttrs, static_cast<DeclaratorContext>(DeclaratorContext::File));
  D.SetIdentifier(csName, Loc);
  ParseScope PrototypeScope(this,Scope::FunctionPrototypeScope|Scope::DeclScope|Scope::FunctionDeclarationScope);
  bool IsAmbiguous = false;
  bool HasProto = true;
  SmallVector<DeclaratorChunk::ParamInfo, 16> ParamInfo;
  SourceLocation EllipsisLoc, RefQualifierLoc; 
  DeclSpec FDS(AttrFactory);
  bool RefQualifierIsLValueRef = true;
  ExceptionSpecificationType ESpecType = EST_None;
  SourceRange ESpecRange;
  SmallVector<ParsedType, 2> DynamicExceptions;
  SmallVector<SourceRange, 2> DynamicExceptionRanges;
  ExprResult NoexceptExpr;
  CachedTokens *ExceptionSpecTokens = 0;
  ParsedAttributes FnAttrs(AttrFactory);
  TypeResult TrailingReturnType;
  ParmVarDecl *Param;
  
  IdentifierInfo *retvalII = CreateIdentifierInfo(__CBC_RETVAL_NAME, Loc);
  Param = CreateParam(retvalII);
  Param->setTypeSourceInfo(CurFuncTI);
  Param->setType(CurFuncResQT);

  ParamInfo.push_back(DeclaratorChunk::ParamInfo(retvalII, Loc, Param, 0));
  IdentifierInfo *envII = CreateIdentifierInfo(__CBC_STRUCT_ENV_NAME, Loc);
  Param = CreateParam(envII, 1, DeclSpec::TST_void);
  ParamInfo.push_back(DeclaratorChunk::ParamInfo(envII, Loc, Param, 0));

  D.AddTypeInfo(DeclaratorChunk::getFunction(HasProto, IsAmbiguous, Loc, ParamInfo.data(), ParamInfo.size(), EllipsisLoc, Loc,
                                             RefQualifierIsLValueRef, RefQualifierLoc, 
                                             SourceLocation(),
                                             ESpecType, ESpecRange.getBegin(),
                                             DynamicExceptions.data(), DynamicExceptionRanges.data(), DynamicExceptions.size(),
                                             NoexceptExpr.isUsable() ? NoexceptExpr.get() : 0, ExceptionSpecTokens, None,
                                             Loc, Loc, D, TrailingReturnType, Loc, &FDS), std::move(FnAttrs), Loc);
  PrototypeScope.Exit();
  
  Decl *TheDecl;
  ParseScope BodyScope(this, Scope::FnScope|Scope::DeclScope);
  Sema::SkipBodyInfo SkipBody;
  const ParsedTemplateInfo &TemplateInfo = ParsedTemplateInfo();
  Decl *BodyRes = Actions.ActOnStartOfFunctionDef(getCurScope(), D,
                                                  TemplateInfo.TemplateParams ? *TemplateInfo.TemplateParams : MultiTemplateParamsArg(),
                                                  &SkipBody);

  D.complete(BodyRes);
  D.getMutableDeclSpec().abort();
  Actions.ActOnDefaultCtorInitializers(BodyRes);
  StmtResult FnBody;
  StmtVector FnStmts;
  StmtResult innerR;
  ExprResult retvalAssginmentExpr,LHS;
  ExprVector ArgExprs;
  CommaLocsTy CommaLocs;
  DeclSpec envDS(AttrFactory);

  if (! isBuiltinSetjmpDefined()) {
      CompileFromString("extern void longjmp(void *,int);",FnStmts); 
  }

  IdentifierInfo *structName = CreateIdentifierInfo(__CBC_STRUCT_NAME, Loc);
  setTST(&envDS, DeclSpec::TST_struct, structName);

  Declarator envDInfo(envDS, ParsedAttributesView::none(), DeclaratorContext::TypeName);
  envDInfo.SetRangeEnd(Loc);
  DeclSpec starDS(AttrFactory);
  starDS.Finish(Actions, Policy);
  envDInfo.SetIdentifier(0,Loc);
  envDInfo.AddTypeInfo(DeclaratorChunk::getPointer(starDS.getTypeQualifiers(), Loc,
                                                   starDS.getConstSpecLoc(),
                                                   starDS.getVolatileSpecLoc(),
                                                   starDS.getRestrictSpecLoc(),
                                                   starDS.getAtomicSpecLoc(),
                                                   starDS.getUnalignedSpecLoc()),
                       std::move(starDS.getAttributes()),
                       SourceLocation());
  {  // scope for RAII
  Sema::CompoundScopeRAII CompoundScope(Actions);
  ExprVector ArgExprs2;
  LHS = LookupNameAndBuildExpr(envII);
  ArgExprs2.push_back(LHS.get());
  LHS = Actions.ActOnParenListExpr(Loc, Loc, ArgExprs2);
  Expr *envCastExpr = LHS.get();
  TypeSourceInfo *castTInfo = Actions.GetTypeForDeclaratorCast(envDInfo, envCastExpr->getType());
  LHS = Actions.MaybeConvertParenListExprToParenExpr(getCurScope(), envCastExpr);
  envCastExpr = LHS.get();
  LHS = Actions.BuildCStyleCastExpr(Loc, castTInfo, Loc, envCastExpr);
  ArgExprs.push_back(LHS.get());	
  LHS = Actions.ActOnParenListExpr(Loc, Loc, ArgExprs);
  LHS = LookupMemberAndBuildExpr(CreateIdentifierInfo(__CBC_STRUCT_POINTER_NAME, Loc),
                                 LHS.get(), true);
  Expr *ret_pCastExpr = LHS.get();
  DeclarationName noValDeclName;
  TypeSourceInfo *CurFuncTypesPointerTI = Actions.Context.CreateTypeSourceInfo(Actions.BuildPointerType(CurFuncResQT, Loc, noValDeclName));
  LHS = Actions.BuildCStyleCastExpr(Loc, CurFuncTypesPointerTI, Loc, ret_pCastExpr);
  LHS = Actions.ActOnUnaryOp(getCurScope(), Loc, tok::star, LHS.get());
  ExprResult RHS;
  RHS = LookupNameAndBuildExpr(retvalII);

  retvalAssginmentExpr = Actions.ActOnBinOp(getCurScope(), Loc, tok::equal, LHS.get(), RHS.get());
  innerR = Actions.ActOnExprStmt(retvalAssginmentExpr);
  if(innerR.isUsable())
    FnStmts.push_back(innerR.get());

  ExprResult ljExpr,ljLHS;
  if (isBuiltinSetjmpDefined()) {
      ljExpr = IIToExpr(CreateIdentifierInfo("__builtin_longjmp",  Loc), tok::l_paren);
  } else {
      ljExpr = IIToExpr(CreateIdentifierInfo("longjmp",  Loc), tok::l_paren);
  }
  ExprVector ljArgExprs;
  DeclSpec ljDS(AttrFactory);
  setTST(&ljDS, DeclSpec::TST_struct, structName);

  Declarator ljD(ljDS, ParsedAttributesView::none(), DeclaratorContext::TypeName);
  ljD.SetRangeEnd(Loc);
  DeclSpec starDS2(AttrFactory);
  starDS2.Finish(Actions, Policy);
  ljD.ExtendWithDeclSpec(starDS2);
  ljD.SetIdentifier(0, Loc);
  ljD.AddTypeInfo(DeclaratorChunk::getPointer(ljDS.getTypeQualifiers(), Loc,
                                              ljDS.getConstSpecLoc(),
                                              ljDS.getVolatileSpecLoc(),
                                              ljDS.getRestrictSpecLoc(),
                                              ljDS.getAtomicSpecLoc(),
                                              ljDS.getUnalignedSpecLoc()),
                  std::move(ljDS.getAttributes()),
                  SourceLocation());
  ljLHS = LookupNameAndBuildExpr(envII);
  Expr *ljCastExpr = ljLHS.get();
  TypeSourceInfo *ljCastTInfo = Actions.GetTypeForDeclaratorCast(ljD, ljCastExpr->getType());
  ljLHS = Actions.BuildCStyleCastExpr(Loc, ljCastTInfo, Loc, ljCastExpr);
  ljLHS = Actions.ActOnParenExpr(Loc, Loc, ljLHS.get());
  ljLHS = LookupMemberAndBuildExpr(envII, ljLHS.get(), true);
  ljLHS = Actions.ActOnParenExpr(Loc, Loc, ljLHS.get());
  ljArgExprs.push_back(ljLHS.get());
  CommaLocs.push_back(Loc);
  ljLHS = Actions.ActOnIntegerConstant(Loc, 1 /* return value for setjmp */);
  ljArgExprs.push_back(ljLHS.get());
  ljExpr = Actions.ActOnCallExpr(getCurScope(), ljExpr.get(), Loc, ljArgExprs, Loc, 0);
  innerR = Actions.ActOnExprStmt(ljExpr);
  if(innerR.isUsable())
    FnStmts.push_back(innerR.get());
  FnBody = Actions.ActOnCompoundStmt(Loc, Loc, FnStmts, false);
  } // RAII clear
  BodyScope.Exit();
  TheDecl = Actions.ActOnFinishFunctionBody(BodyRes, FnBody.get());
  returnDecl =  Actions.ConvertDeclToDeclGroup(TheDecl);
  (&Actions.getASTConsumer())->HandleTopLevelDecl(returnDecl.get());
  Actions.CurScope = SavedScope;
  Actions.CurContext = SavedContext;
  Actions.FunctionScopes.push_back(SavedFSI);
}

/// IIToExpr - Create ExprResult from IdentifierInfo. 
/// It is used when II is a not primary expression such as not primary types, a function's name, etc.
ExprResult Parser::IIToExpr(IdentifierInfo *II, tok::TokenKind Kind){
  SourceLocation Loc = Tok.getLocation();
  Token Next,IITok;
  Next.setKind(Kind);
  ExternalSpace::StatementFilterCCC CCCValidator(Next);
  CXXScopeSpec SS;
  Sema::NameClassification Classification = Actions.ClassifyName(getCurScope(), SS, II, Loc, Next, &CCCValidator);
  IITok.startToken();
  IITok.setLocation(Loc);
  IITok.setIdentifierInfo(II);
  IITok.setKind(tok::annot_primary_expr);
  if (Classification.getKind() == Sema::NC_NonType) {
      IITok.setKind(tok::annot_non_type);
      setNonTypeAnnotation(IITok, Classification.getNonTypeDecl());
      IITok.setLocation(Loc);
      IITok.setAnnotationEndLoc(Loc);
      PP.AnnotateCachedTokens(IITok);
      const bool WasScopeAnnotation = Tok.is(tok::annot_cxxscope);
      if (SS.isNotEmpty())
          AnnotateScopeToken(SS, !WasScopeAnnotation);
      NamedDecl *ND = getNonTypeAnnotation(IITok);
      ExprResult E;
      return Actions.ActOnNameClassifiedAsNonType(getCurScope(), SS, ND, Loc, IITok);
  }
  setExprAnnotation(IITok, Classification.getExpression());
  IITok.setAnnotationEndLoc(Loc);
  PP.AnnotateCachedTokens(IITok);
  return getExprAnnotation(IITok);
}

/// CreateComplexStmtRet - Create return value for complex statements.
///
///   ({ /* some statements */
///      return_value; )};
///     ^^^^^^^^^^^^^ Create it.
StmtResult Parser::CreateComplexStmtRet(IdentifierInfo *II, bool IsAddressOfOperand){
  ExprResult ER;
  if (IsAddressOfOperand) {
    ER = LookupNameAndBuildExpr(II, true);
    ER = Actions.ActOnUnaryOp(getCurScope(), Tok.getLocation(), tok::amp, ER.get());
  }
  else
    ER = IIToExpr(II,tok::semi);
  return Actions.ActOnExprStmt(ER,false);
}

/// CreateParam - Create paramator for functions.
/// 
/// int funcname(int aua) { 
///              ^^^^^^^ Create it.
ParmVarDecl* Parser::CreateParam(IdentifierInfo *II, int pointerNum, DeclSpec::TST T){
  const PrintingPolicy &Policy = Actions.getASTContext().getPrintingPolicy();
  SourceLocation Loc = Tok.getLocation();
  DeclSpec DS(AttrFactory);
  setTST(&DS, T);
  Declarator ParamDeclarator(DS, ParsedAttributesView::none(), DeclaratorContext::Prototype);
  ParamDeclarator.SetIdentifier(II, Loc);
  for(int i = 0;i<pointerNum; i++){
    DeclSpec pointerDS(AttrFactory);
    pointerDS.Finish(Actions, Policy);
    ParamDeclarator.AddTypeInfo(DeclaratorChunk::getPointer(pointerDS.getTypeQualifiers(), Loc,
                                                            pointerDS.getConstSpecLoc(),
                                                            pointerDS.getVolatileSpecLoc(),
                                                            pointerDS.getRestrictSpecLoc(),
                                                            pointerDS.getAtomicSpecLoc(),
                                                            pointerDS.getUnalignedSpecLoc()),
                                std::move(pointerDS.getAttributes()),SourceLocation());
  }
  ParmVarDecl *Param = dyn_cast<ParmVarDecl>(Actions.ActOnParamDeclarator(getCurScope(), ParamDeclarator));
  return Param;

}

/// setTST - set TypeSpecifierType(TST) DeclSpec.
/// TST is specifiers the kind of type such as int, double, char, etc.
void Parser::setTST(DeclSpec *DS, DeclSpec::TST T, IdentifierInfo* Name, DeclSpec::TQ TQ){
  const PrintingPolicy &Policy = Actions.getASTContext().getPrintingPolicy();
  SourceLocation Loc = Tok.getLocation();
  bool isInvalid = false;
  const char *PrevSpec = 0;
  unsigned DiagID = 0;
  CXXScopeSpec SS;
  DS->SetRangeStart(Loc);
  DS->SetRangeEnd(Loc);
  if (TQ != DeclSpec::TQ_unspecified) {
    isInvalid = DS->SetTypeQual(DeclSpec::TQ_volatile, Loc, PrevSpec, DiagID,
                               getLangOpts());
  }

  if (T == DeclSpec::TST_struct) {
    ParsedAttributes attrs(AttrFactory);
    DeclResult TagOrTempResult = true;
    bool Owned = false;
    bool IsDependent = false;
    MultiTemplateParamsArg TParams;
    TagOrTempResult = Actions.ActOnTag(getCurScope(), T, Sema::TUK_Reference, Loc,
                                       SS, Name, Loc, attrs, AS_none,
                                       DS->getModulePrivateSpecLoc(),
                                       TParams, Owned, IsDependent,
                                       SourceLocation(), false,
                                       clang::TypeResult(), false, false);
    isInvalid = DS->SetTypeSpecType(T, Loc, Loc, PrevSpec, DiagID, TagOrTempResult.get(), Owned, Policy);
  }
  else if (T == DeclSpec::TST_typename) {
    Token Next,TypeTok;
    Next.setKind(tok::identifier);
    ExternalSpace::StatementFilterCCC CCCValidator(Next);
    Sema::NameClassification Classification = Actions.ClassifyName(getCurScope(), SS, Name, Loc, Next, &CCCValidator);
    TypeTok.startToken();
    TypeTok.setLocation(Loc);
    TypeTok.setIdentifierInfo(Name);
    TypeTok.setKind(tok::annot_typename);
    setTypeAnnotation(TypeTok, Classification.getType());
    TypeTok.setAnnotationEndLoc(Loc);
    PP.AnnotateCachedTokens(TypeTok);
    if (TypeTok.getAnnotationValue()) {
      TypeResult TR = getTypeAnnotation(TypeTok);
      isInvalid = DS->SetTypeSpecType(T, Loc, PrevSpec, DiagID, TR, Policy);
    } else
      DS->SetTypeSpecError();
  }
  else
    isInvalid = DS->SetTypeSpecType(T, Loc, PrevSpec, DiagID, Policy);
  
  DS->Finish(Actions, Policy);
  if (isInvalid) {
    assert(PrevSpec && "Method did not return previous specifier!");
    assert(DiagID);
    if (DiagID == diag::ext_duplicate_declspec)
      Diag(Tok, DiagID)
        << PrevSpec << FixItHint::CreateRemoval(Tok.getLocation());
    else
      Diag(Tok, DiagID) << PrevSpec;
  }
}

/// CheckTheSjHeader - Check whether setjmp.h has been already included or not.
/// If not, include it.
void Parser::CheckTheSjHeader(){
#if 0
  SourceLocation Loc = Tok.getLocation();
  LookupResult R(Actions, CreateIdentifierInfo("setjmp", Loc), Loc, Actions.LookupOrdinaryName, Actions.ForVisibleRedeclaration);
  if (!Actions.LookupName(R, getCurScope())){ // look up the setjmp
    if (PP.IncludeHeader(Tok, "setjmp.h"))
      ConsumeToken();
  }
#endif
}

/// isVoidFunction - Return true if current function return type is void.
bool Parser::isVoidFunction(){
  return Actions.getCurFunctionDecl()->getReturnType().getTypePtr()->isVoidType();
}

/// ParseCbCGotoStatement
///       jump-statement:
/// [CbC]   'goto' codeSegment ';'
///
StmtResult Parser::ParseCbCGotoStatement(ParsedAttributes &Attrs,StmtVector &Stmts) {
  assert(Tok.is(tok::kw_goto) && "Not a goto stmt!");
  StmtVector CompoundedStmts;

  SourceLocation gotoLoc = ConsumeToken();  // eat the 'goto'.
  StmtResult gotoRes;
  Token TokAfterGoto = Tok;
  Stmtsp = &Stmts;

  ParsedStmtContext SubStmtCtx =
     ParsedStmtContext::Compound  |  ParsedStmtContext::InStmtExpr; // unconfident
  gotoRes = ParseStatementOrDeclaration(Stmts, SubStmtCtx);

  if (gotoRes.get() == NULL)
    return StmtError();
  else if (gotoRes.get()->getStmtClass() != Stmt::CallExprClass) { // if it is not function call
    unsigned DiagID = Diags.getCustomDiagID(DiagnosticsEngine::Error, "expected identifier or codesegment call");
    Diag(TokAfterGoto, DiagID);
    return StmtError();
  }
  
  assert((Attrs.empty() || gotoRes.isInvalid() || gotoRes.isUsable()) &&
         "attributes on empty statement");
  if (!(Attrs.empty() || gotoRes.isInvalid()))
    gotoRes = Actions.ActOnAttributedStmt(Attrs,gotoRes.get());
  if (gotoRes.isUsable())
    CompoundedStmts.push_back(gotoRes.get());

  // add return; after goto codesegment();
  if (Actions.getCurFunctionDecl()->getReturnType().getTypePtr()->is__CodeType()) {
    ExprResult retExpr;
    StmtResult retRes;
    retRes = Actions.ActOnReturnStmt(gotoLoc, retExpr.get(), getCurScope());
    if (retRes.isUsable())
      CompoundedStmts.push_back(retRes.get());
  }
  Sema::CompoundScopeRAII CompoundScope(Actions);
  StmtResult R =  Actions.ActOnCompoundStmt(gotoLoc, Tok.getLocation(), CompoundedStmts, false);
  return R;
}

/// SearchCodeSegmentDeclaration - Read tokens until we get to the specified code segment declaration.
/// If we can't find it , return false;
bool Parser::SearchCodeSegmentDeclaration(std::string Name){
  while(SkipAnyUntil(tok::kw___code, StopBeforeMatch)){
    if(NextToken().is(tok::identifier) && NextToken().getIdentifierInfo()->getName().str() == Name)
      return true;
    ConsumeToken();
  }
  return false;
}

static bool HasFlagsSet(Parser::SkipUntilFlags L, Parser::SkipUntilFlags R) {
  return (static_cast<unsigned>(L) & static_cast<unsigned>(R)) != 0;
}

bool Parser::SkipAnyUntil(tok::TokenKind T, SkipUntilFlags Flags){
  const PreprocessorLexer *L = PP.getCurrentFileLexer();
  while(1){
    if(Tok.is(T)){
      if (HasFlagsSet(Flags, StopBeforeMatch)) {
        // Noop, don't consume the token.
      } else {
        ConsumeAnyToken();
      }
      return true;
    }
    else if(PP.getCurrentFileLexer() != L){
      return false;
    }

    ConsumeAnyToken();
  }
}

//
// you may get Actions.getASTContext().
//
std::string get_string(const Expr *expr, const ASTContext &Context) {
  PrintingPolicy print_policy(Context.getLangOpts());
  print_policy.FullyQualifiedName = 1;
  print_policy.SuppressScope = 0;
  print_policy.SuppressUnwrittenScope = 0;
  std::string expr_string;
  llvm::raw_string_ostream stream(expr_string);
  expr->printPretty(stream, nullptr, print_policy);
  stream.flush();
  return expr_string;
}

std::string get_string(const Stmt *expr, const ASTContext &Context) {
  PrintingPolicy print_policy(Context.getLangOpts());
  print_policy.FullyQualifiedName = 1;
  print_policy.SuppressScope = 0;
  print_policy.SuppressUnwrittenScope = 0;
  std::string expr_string;
  llvm::raw_string_ostream stream(expr_string);
  expr->printPretty(stream, nullptr, print_policy);
  stream.flush();
  return expr_string;
}

#endif
