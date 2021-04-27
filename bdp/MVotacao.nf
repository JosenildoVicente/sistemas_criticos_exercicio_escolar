Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(MVotacao))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(MVotacao))==(Machine(MVotacao));
  Level(Machine(MVotacao))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(MVotacao)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(MVotacao))==(MVotacao_Ctx)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(MVotacao))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(MVotacao))==(?);
  List_Includes(Machine(MVotacao))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(MVotacao))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(MVotacao))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(MVotacao))==(?);
  Context_List_Variables(Machine(MVotacao))==(?);
  Abstract_List_Variables(Machine(MVotacao))==(?);
  Local_List_Variables(Machine(MVotacao))==(registro,votos,local,candidatos,eleitores,estado);
  List_Variables(Machine(MVotacao))==(registro,votos,local,candidatos,eleitores,estado);
  External_List_Variables(Machine(MVotacao))==(registro,votos,local,candidatos,eleitores,estado)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(MVotacao))==(?);
  Abstract_List_VisibleVariables(Machine(MVotacao))==(?);
  External_List_VisibleVariables(Machine(MVotacao))==(?);
  Expanded_List_VisibleVariables(Machine(MVotacao))==(?);
  List_VisibleVariables(Machine(MVotacao))==(?);
  Internal_List_VisibleVariables(Machine(MVotacao))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(MVotacao))==(btrue);
  Gluing_List_Invariant(Machine(MVotacao))==(btrue);
  Expanded_List_Invariant(Machine(MVotacao))==(btrue);
  Abstract_List_Invariant(Machine(MVotacao))==(btrue);
  Context_List_Invariant(Machine(MVotacao))==(btrue);
  List_Invariant(Machine(MVotacao))==(estado: ESTADO & eleitores: FIN(CPFs) & candidatos: FIN(CPFs) & local: eleitores +-> ZONAS & votos: candidatos +-> NAT & registro: eleitores +-> BOOL & !aa.(aa: eleitores => aa/=indefinido) & !ee.(ee: candidatos => ee/=indefinido) & !ii.(ii: candidatos => ii: eleitores) & !oo.(oo: ran(registro) & estado = preparacao => oo = FALSE) & !uu.(uu: ran(votos) & estado = preparacao => uu = 0))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(MVotacao))==(btrue);
  Abstract_List_Assertions(Machine(MVotacao))==(btrue);
  Context_List_Assertions(Machine(MVotacao))==(btrue);
  List_Assertions(Machine(MVotacao))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(MVotacao))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(MVotacao))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(MVotacao))==(estado,eleitores,candidatos,local,votos,registro:=preparacao,{},{},{},{},{});
  Context_List_Initialisation(Machine(MVotacao))==(skip);
  List_Initialisation(Machine(MVotacao))==(estado:=preparacao || eleitores:={} || candidatos:={} || local:={} || votos:={} || registro:={})
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(MVotacao))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(MVotacao),Machine(MVotacao_Ctx))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(MVotacao))==(btrue);
  List_Constraints(Machine(MVotacao))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(MVotacao))==(adicionar_eleitor,remover_eleitor);
  List_Operations(Machine(MVotacao))==(adicionar_eleitor,remover_eleitor)
END
&
THEORY ListInputX IS
  List_Input(Machine(MVotacao),adicionar_eleitor)==(cpf,zona);
  List_Input(Machine(MVotacao),remover_eleitor)==(cpf)
END
&
THEORY ListOutputX IS
  List_Output(Machine(MVotacao),adicionar_eleitor)==(?);
  List_Output(Machine(MVotacao),remover_eleitor)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(MVotacao),adicionar_eleitor)==(adicionar_eleitor(cpf,zona));
  List_Header(Machine(MVotacao),remover_eleitor)==(remover_eleitor(cpf))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(MVotacao),adicionar_eleitor)==(estado = preparacao & cpf: CPFs & cpf/:eleitores & zona: ZONAS & cpf/=indefinido);
  List_Precondition(Machine(MVotacao),remover_eleitor)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(MVotacao),remover_eleitor)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido | cpf: candidatos ==> votos,candidatos:={cpf}<<|votos,candidatos-{cpf} [] not(cpf: candidatos) ==> skip || local:={cpf}<<|local || registro:={cpf}<<|registro || eleitores:=eleitores-{cpf});
  Expanded_List_Substitution(Machine(MVotacao),adicionar_eleitor)==(estado = preparacao & cpf: CPFs & cpf/:eleitores & zona: ZONAS & cpf/=indefinido | eleitores,local,registro:=eleitores\/{cpf},local\/{cpf|->zona},registro\/{cpf|->FALSE});
  List_Substitution(Machine(MVotacao),adicionar_eleitor)==(eleitores:=eleitores\/{cpf} || local:=local\/{cpf|->zona} || registro:=registro\/{cpf|->FALSE});
  List_Substitution(Machine(MVotacao),remover_eleitor)==(IF cpf: candidatos THEN votos:={cpf}<<|votos || candidatos:=candidatos-{cpf} END || local:={cpf}<<|local || registro:={cpf}<<|registro || eleitores:=eleitores-{cpf})
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(MVotacao))==(?);
  Inherited_List_Constants(Machine(MVotacao))==(?);
  List_Constants(Machine(MVotacao))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(MVotacao),ZONAS)==({zona1,zona2});
  Context_List_Enumerated(Machine(MVotacao))==(ESTADO,CPFs,ZONAS);
  Context_List_Defered(Machine(MVotacao))==(?);
  Context_List_Sets(Machine(MVotacao))==(ESTADO,CPFs,ZONAS);
  List_Valuable_Sets(Machine(MVotacao))==(?);
  Inherited_List_Enumerated(Machine(MVotacao))==(?);
  Inherited_List_Defered(Machine(MVotacao))==(?);
  Inherited_List_Sets(Machine(MVotacao))==(?);
  List_Enumerated(Machine(MVotacao))==(?);
  List_Defered(Machine(MVotacao))==(?);
  List_Sets(Machine(MVotacao))==(?);
  Set_Definition(Machine(MVotacao),CPFs)==({cpf1,cpf2,cpf3,indefinido});
  Set_Definition(Machine(MVotacao),ESTADO)==({preparacao,votacao,apuracao})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(MVotacao))==(?);
  Expanded_List_HiddenConstants(Machine(MVotacao))==(?);
  List_HiddenConstants(Machine(MVotacao))==(?);
  External_List_HiddenConstants(Machine(MVotacao))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(MVotacao))==(btrue);
  Context_List_Properties(Machine(MVotacao))==(ESTADO: FIN(INTEGER) & not(ESTADO = {}) & CPFs: FIN(INTEGER) & not(CPFs = {}) & ZONAS: FIN(INTEGER) & not(ZONAS = {}));
  Inherited_List_Properties(Machine(MVotacao))==(btrue);
  List_Properties(Machine(MVotacao))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(MVotacao),Machine(MVotacao_Ctx))==(?);
  Seen_Context_List_Enumerated(Machine(MVotacao))==(?);
  Seen_Context_List_Invariant(Machine(MVotacao))==(btrue);
  Seen_Context_List_Assertions(Machine(MVotacao))==(btrue);
  Seen_Context_List_Properties(Machine(MVotacao))==(btrue);
  Seen_List_Constraints(Machine(MVotacao))==(btrue);
  Seen_List_Operations(Machine(MVotacao),Machine(MVotacao_Ctx))==(?);
  Seen_Expanded_List_Invariant(Machine(MVotacao),Machine(MVotacao_Ctx))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(MVotacao),adicionar_eleitor)==(?);
  List_ANY_Var(Machine(MVotacao),remover_eleitor)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(MVotacao)) == (? | ? | registro,votos,local,candidatos,eleitores,estado | ? | adicionar_eleitor,remover_eleitor | ? | seen(Machine(MVotacao_Ctx)) | ? | MVotacao);
  List_Of_HiddenCst_Ids(Machine(MVotacao)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MVotacao)) == (?);
  List_Of_VisibleVar_Ids(Machine(MVotacao)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MVotacao)) == (?: ?);
  List_Of_Ids(Machine(MVotacao_Ctx)) == (ESTADO,CPFs,ZONAS,preparacao,votacao,apuracao,cpf1,cpf2,cpf3,indefinido,zona1,zona2 | ? | ? | ? | ? | ? | ? | ? | MVotacao_Ctx);
  List_Of_HiddenCst_Ids(Machine(MVotacao_Ctx)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MVotacao_Ctx)) == (?);
  List_Of_VisibleVar_Ids(Machine(MVotacao_Ctx)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MVotacao_Ctx)) == (?: ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(MVotacao)) == (Type(registro) == Mvl(SetOf(etype(CPFs,?,?)*btype(BOOL,?,?)));Type(votos) == Mvl(SetOf(etype(CPFs,?,?)*btype(INTEGER,?,?)));Type(local) == Mvl(SetOf(etype(CPFs,?,?)*etype(ZONAS,?,?)));Type(candidatos) == Mvl(SetOf(etype(CPFs,?,?)));Type(eleitores) == Mvl(SetOf(etype(CPFs,?,?)));Type(estado) == Mvl(etype(ESTADO,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(MVotacao)) == (Type(remover_eleitor) == Cst(No_type,etype(CPFs,?,?));Type(adicionar_eleitor) == Cst(No_type,etype(CPFs,?,?)*etype(ZONAS,?,?)))
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
