Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(MVotacao_Ctx))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(MVotacao_Ctx))==(Machine(MVotacao_Ctx));
  Level(Machine(MVotacao_Ctx))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(MVotacao_Ctx)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(MVotacao_Ctx))==(?);
  List_Includes(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(MVotacao_Ctx))==(?);
  Context_List_Variables(Machine(MVotacao_Ctx))==(?);
  Abstract_List_Variables(Machine(MVotacao_Ctx))==(?);
  Local_List_Variables(Machine(MVotacao_Ctx))==(?);
  List_Variables(Machine(MVotacao_Ctx))==(?);
  External_List_Variables(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(MVotacao_Ctx))==(?);
  Abstract_List_VisibleVariables(Machine(MVotacao_Ctx))==(?);
  External_List_VisibleVariables(Machine(MVotacao_Ctx))==(?);
  Expanded_List_VisibleVariables(Machine(MVotacao_Ctx))==(?);
  List_VisibleVariables(Machine(MVotacao_Ctx))==(?);
  Internal_List_VisibleVariables(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(MVotacao_Ctx))==(btrue);
  Gluing_List_Invariant(Machine(MVotacao_Ctx))==(btrue);
  Expanded_List_Invariant(Machine(MVotacao_Ctx))==(btrue);
  Abstract_List_Invariant(Machine(MVotacao_Ctx))==(btrue);
  Context_List_Invariant(Machine(MVotacao_Ctx))==(btrue);
  List_Invariant(Machine(MVotacao_Ctx))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(MVotacao_Ctx))==(btrue);
  Abstract_List_Assertions(Machine(MVotacao_Ctx))==(btrue);
  Context_List_Assertions(Machine(MVotacao_Ctx))==(btrue);
  List_Assertions(Machine(MVotacao_Ctx))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(MVotacao_Ctx))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(MVotacao_Ctx))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(MVotacao_Ctx))==(skip);
  Context_List_Initialisation(Machine(MVotacao_Ctx))==(skip);
  List_Initialisation(Machine(MVotacao_Ctx))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(MVotacao_Ctx))==(btrue);
  List_Constraints(Machine(MVotacao_Ctx))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(MVotacao_Ctx))==(?);
  List_Operations(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListInputX END
&
THEORY ListOutputX END
&
THEORY ListHeaderX END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX END
&
THEORY ListSubstitutionX END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(MVotacao_Ctx))==(?);
  Inherited_List_Constants(Machine(MVotacao_Ctx))==(?);
  List_Constants(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(MVotacao_Ctx),ESTADO)==({preparacao,votacao,apuracao});
  Context_List_Enumerated(Machine(MVotacao_Ctx))==(?);
  Context_List_Defered(Machine(MVotacao_Ctx))==(?);
  Context_List_Sets(Machine(MVotacao_Ctx))==(?);
  List_Valuable_Sets(Machine(MVotacao_Ctx))==(?);
  Inherited_List_Enumerated(Machine(MVotacao_Ctx))==(?);
  Inherited_List_Defered(Machine(MVotacao_Ctx))==(?);
  Inherited_List_Sets(Machine(MVotacao_Ctx))==(?);
  List_Enumerated(Machine(MVotacao_Ctx))==(ESTADO,CPFs,ZONAS);
  List_Defered(Machine(MVotacao_Ctx))==(?);
  List_Sets(Machine(MVotacao_Ctx))==(ESTADO,CPFs,ZONAS);
  Set_Definition(Machine(MVotacao_Ctx),CPFs)==({cpf1,cpf2,cpf3,indefinido});
  Set_Definition(Machine(MVotacao_Ctx),ZONAS)==({zona1,zona2})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(MVotacao_Ctx))==(?);
  Expanded_List_HiddenConstants(Machine(MVotacao_Ctx))==(?);
  List_HiddenConstants(Machine(MVotacao_Ctx))==(?);
  External_List_HiddenConstants(Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(MVotacao_Ctx))==(btrue);
  Context_List_Properties(Machine(MVotacao_Ctx))==(btrue);
  Inherited_List_Properties(Machine(MVotacao_Ctx))==(btrue);
  List_Properties(Machine(MVotacao_Ctx))==(ESTADO: FIN(INTEGER) & not(ESTADO = {}) & CPFs: FIN(INTEGER) & not(CPFs = {}) & ZONAS: FIN(INTEGER) & not(ZONAS = {}))
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(MVotacao_Ctx)) == (ESTADO,CPFs,ZONAS,preparacao,votacao,apuracao,cpf1,cpf2,cpf3,indefinido,zona1,zona2 | ? | ? | ? | ? | ? | ? | ? | MVotacao_Ctx);
  List_Of_HiddenCst_Ids(Machine(MVotacao_Ctx)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MVotacao_Ctx)) == (?);
  List_Of_VisibleVar_Ids(Machine(MVotacao_Ctx)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MVotacao_Ctx)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(MVotacao_Ctx)) == (Type(ESTADO) == Cst(SetOf(etype(ESTADO,0,2)));Type(CPFs) == Cst(SetOf(etype(CPFs,0,3)));Type(ZONAS) == Cst(SetOf(etype(ZONAS,0,1))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(MVotacao_Ctx)) == (Type(preparacao) == Cst(etype(ESTADO,0,2));Type(votacao) == Cst(etype(ESTADO,0,2));Type(apuracao) == Cst(etype(ESTADO,0,2));Type(cpf1) == Cst(etype(CPFs,0,3));Type(cpf2) == Cst(etype(CPFs,0,3));Type(cpf3) == Cst(etype(CPFs,0,3));Type(indefinido) == Cst(etype(CPFs,0,3));Type(zona1) == Cst(etype(ZONAS,0,1));Type(zona2) == Cst(etype(ZONAS,0,1)))
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
)
