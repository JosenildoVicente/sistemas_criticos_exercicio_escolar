Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(MVotacao_Ctx_i))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(MVotacao_Ctx_i))==(Machine(MVotacao_Ctx));
  Level(Implementation(MVotacao_Ctx_i))==(1);
  Upper_Level(Implementation(MVotacao_Ctx_i))==(Machine(MVotacao_Ctx))
END
&
THEORY LoadedStructureX IS
  Implementation(MVotacao_Ctx_i)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(MVotacao_Ctx_i))==(?);
  Inherited_List_Includes(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(MVotacao_Ctx_i))==(?);
  Context_List_Variables(Implementation(MVotacao_Ctx_i))==(?);
  Abstract_List_Variables(Implementation(MVotacao_Ctx_i))==(?);
  Local_List_Variables(Implementation(MVotacao_Ctx_i))==(?);
  List_Variables(Implementation(MVotacao_Ctx_i))==(?);
  External_List_Variables(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(MVotacao_Ctx_i))==(?);
  Abstract_List_VisibleVariables(Implementation(MVotacao_Ctx_i))==(?);
  External_List_VisibleVariables(Implementation(MVotacao_Ctx_i))==(?);
  Expanded_List_VisibleVariables(Implementation(MVotacao_Ctx_i))==(?);
  List_VisibleVariables(Implementation(MVotacao_Ctx_i))==(?);
  Internal_List_VisibleVariables(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(MVotacao_Ctx_i))==(btrue);
  Expanded_List_Invariant(Implementation(MVotacao_Ctx_i))==(btrue);
  Abstract_List_Invariant(Implementation(MVotacao_Ctx_i))==(btrue);
  Context_List_Invariant(Implementation(MVotacao_Ctx_i))==(btrue);
  List_Invariant(Implementation(MVotacao_Ctx_i))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Implementation(MVotacao_Ctx_i))==(btrue);
  Abstract_List_Assertions(Implementation(MVotacao_Ctx_i))==(btrue);
  Context_List_Assertions(Implementation(MVotacao_Ctx_i))==(btrue);
  List_Assertions(Implementation(MVotacao_Ctx_i))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Implementation(MVotacao_Ctx_i))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Implementation(MVotacao_Ctx_i))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(MVotacao_Ctx_i))==(skip);
  Context_List_Initialisation(Implementation(MVotacao_Ctx_i))==(skip);
  List_Initialisation(Implementation(MVotacao_Ctx_i))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(MVotacao_Ctx_i))==(btrue);
  List_Context_Constraints(Implementation(MVotacao_Ctx_i))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(MVotacao_Ctx_i))==(?);
  List_Operations(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListInputX END
&
THEORY ListOutputX END
&
THEORY ListHeaderX END
&
THEORY ListPreconditionX END
&
THEORY ListSubstitutionX END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(MVotacao_Ctx_i))==(?);
  Inherited_List_Constants(Implementation(MVotacao_Ctx_i))==(?);
  List_Constants(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Implementation(MVotacao_Ctx_i),ZONAS)==({zona1,zona2});
  Context_List_Enumerated(Implementation(MVotacao_Ctx_i))==(?);
  Context_List_Defered(Implementation(MVotacao_Ctx_i))==(?);
  Context_List_Sets(Implementation(MVotacao_Ctx_i))==(?);
  List_Own_Enumerated(Implementation(MVotacao_Ctx_i))==(ESTADO,CPFs,ZONAS);
  List_Valuable_Sets(Implementation(MVotacao_Ctx_i))==(?);
  Inherited_List_Enumerated(Implementation(MVotacao_Ctx_i))==(ESTADO,CPFs,ZONAS);
  Inherited_List_Defered(Implementation(MVotacao_Ctx_i))==(?);
  Inherited_List_Sets(Implementation(MVotacao_Ctx_i))==(ESTADO,CPFs,ZONAS);
  List_Enumerated(Implementation(MVotacao_Ctx_i))==(?);
  List_Defered(Implementation(MVotacao_Ctx_i))==(?);
  List_Sets(Implementation(MVotacao_Ctx_i))==(?);
  Set_Definition(Implementation(MVotacao_Ctx_i),CPFs)==({cpf1,cpf2,cpf3,indefinido});
  Set_Definition(Implementation(MVotacao_Ctx_i),ESTADO)==({preparacao,votacao,apuracao})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(MVotacao_Ctx_i))==(?);
  Expanded_List_HiddenConstants(Implementation(MVotacao_Ctx_i))==(?);
  List_HiddenConstants(Implementation(MVotacao_Ctx_i))==(?);
  External_List_HiddenConstants(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(MVotacao_Ctx_i))==(ESTADO: FIN(INTEGER) & not(ESTADO = {}) & CPFs: FIN(INTEGER) & not(CPFs = {}) & ZONAS: FIN(INTEGER) & not(ZONAS = {}));
  Context_List_Properties(Implementation(MVotacao_Ctx_i))==(btrue);
  Inherited_List_Properties(Implementation(MVotacao_Ctx_i))==(btrue);
  List_Properties(Implementation(MVotacao_Ctx_i))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(MVotacao_Ctx_i))==(aa: aa);
  List_Values(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListSeenInfoX END
&
THEORY ListIncludedOperationsX END
&
THEORY InheritedEnvX IS
  Constants(Implementation(MVotacao_Ctx_i))==(Type(preparacao) == Cst(etype(ESTADO,0,2));Type(votacao) == Cst(etype(ESTADO,0,2));Type(apuracao) == Cst(etype(ESTADO,0,2));Type(cpf1) == Cst(etype(CPFs,0,3));Type(cpf2) == Cst(etype(CPFs,0,3));Type(cpf3) == Cst(etype(CPFs,0,3));Type(indefinido) == Cst(etype(CPFs,0,3));Type(zona1) == Cst(etype(ZONAS,0,1));Type(zona2) == Cst(etype(ZONAS,0,1)))
END
&
THEORY ListVisibleStaticX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(MVotacao_Ctx_i)) == (? | ? | ? | ? | ? | ? | ? | ? | MVotacao_Ctx_i);
  List_Of_HiddenCst_Ids(Implementation(MVotacao_Ctx_i)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(MVotacao_Ctx_i)) == (?);
  List_Of_VisibleVar_Ids(Implementation(MVotacao_Ctx_i)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(MVotacao_Ctx_i)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Implementation(MVotacao_Ctx_i)) == (Type(ZONAS) == Cst(SetOf(etype(ZONAS,0,1)));Type(CPFs) == Cst(SetOf(etype(CPFs,0,3)));Type(ESTADO) == Cst(SetOf(etype(ESTADO,0,2))))
END
&
THEORY ConstantsEnvX IS
  Constants(Implementation(MVotacao_Ctx_i)) == (Type(zona2) == Cst(etype(ZONAS,0,1));Type(zona1) == Cst(etype(ZONAS,0,1));Type(indefinido) == Cst(etype(CPFs,0,3));Type(cpf3) == Cst(etype(CPFs,0,3));Type(cpf2) == Cst(etype(CPFs,0,3));Type(cpf1) == Cst(etype(CPFs,0,3));Type(apuracao) == Cst(etype(ESTADO,0,2));Type(votacao) == Cst(etype(ESTADO,0,2));Type(preparacao) == Cst(etype(ESTADO,0,2)))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
&
THEORY ListLocalOperationsX IS
  List_Local_Operations(Implementation(MVotacao_Ctx_i))==(?)
END
&
THEORY ListLocalInputX END
&
THEORY ListLocalOutputX END
&
THEORY ListLocalHeaderX END
&
THEORY ListLocalPreconditionX END
&
THEORY ListLocalSubstitutionX END
&
THEORY TypingPredicateX IS
  TypingPredicate(Implementation(MVotacao_Ctx_i))==(btrue)
END
&
THEORY ImportedVariablesListX END
&
THEORY ListLocalOpInvariantX END
)
