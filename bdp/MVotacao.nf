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
  Internal_List_Operations(Machine(MVotacao))==(adicionar_eleitor,remover_eleitor,mudar_local,adicionar_candidato,remover_candidato,iniciar_votacao,votar,concluir_votacao,apurar_vencedor);
  List_Operations(Machine(MVotacao))==(adicionar_eleitor,remover_eleitor,mudar_local,adicionar_candidato,remover_candidato,iniciar_votacao,votar,concluir_votacao,apurar_vencedor)
END
&
THEORY ListInputX IS
  List_Input(Machine(MVotacao),adicionar_eleitor)==(cpf,zona);
  List_Input(Machine(MVotacao),remover_eleitor)==(cpf);
  List_Input(Machine(MVotacao),mudar_local)==(cpf,zona);
  List_Input(Machine(MVotacao),adicionar_candidato)==(cpf);
  List_Input(Machine(MVotacao),remover_candidato)==(cpf);
  List_Input(Machine(MVotacao),iniciar_votacao)==(?);
  List_Input(Machine(MVotacao),votar)==(cpf,zona,cpf_candidato);
  List_Input(Machine(MVotacao),concluir_votacao)==(?);
  List_Input(Machine(MVotacao),apurar_vencedor)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(MVotacao),adicionar_eleitor)==(?);
  List_Output(Machine(MVotacao),remover_eleitor)==(?);
  List_Output(Machine(MVotacao),mudar_local)==(?);
  List_Output(Machine(MVotacao),adicionar_candidato)==(?);
  List_Output(Machine(MVotacao),remover_candidato)==(?);
  List_Output(Machine(MVotacao),iniciar_votacao)==(?);
  List_Output(Machine(MVotacao),votar)==(?);
  List_Output(Machine(MVotacao),concluir_votacao)==(?);
  List_Output(Machine(MVotacao),apurar_vencedor)==(resultado)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(MVotacao),adicionar_eleitor)==(adicionar_eleitor(cpf,zona));
  List_Header(Machine(MVotacao),remover_eleitor)==(remover_eleitor(cpf));
  List_Header(Machine(MVotacao),mudar_local)==(mudar_local(cpf,zona));
  List_Header(Machine(MVotacao),adicionar_candidato)==(adicionar_candidato(cpf));
  List_Header(Machine(MVotacao),remover_candidato)==(remover_candidato(cpf));
  List_Header(Machine(MVotacao),iniciar_votacao)==(iniciar_votacao);
  List_Header(Machine(MVotacao),votar)==(votar(cpf,zona,cpf_candidato));
  List_Header(Machine(MVotacao),concluir_votacao)==(concluir_votacao);
  List_Header(Machine(MVotacao),apurar_vencedor)==(resultado <-- apurar_vencedor)
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(MVotacao),adicionar_eleitor)==(estado = preparacao & cpf: CPFs & cpf/:eleitores & zona: ZONAS & cpf/=indefinido);
  List_Precondition(Machine(MVotacao),remover_eleitor)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido);
  List_Precondition(Machine(MVotacao),mudar_local)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf: dom(local) & zona: ZONAS & zona/=local(cpf) & cpf/=indefinido);
  List_Precondition(Machine(MVotacao),adicionar_candidato)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & cpf/:candidatos);
  List_Precondition(Machine(MVotacao),remover_candidato)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & cpf: candidatos);
  List_Precondition(Machine(MVotacao),iniciar_votacao)==(estado = preparacao & candidatos/={});
  List_Precondition(Machine(MVotacao),votar)==(estado = votacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & zona: ZONAS & cpf: dom(local) & cpf: dom(registro) & zona = local(cpf) & cpf_candidato: CPFs & cpf_candidato: candidatos\/{indefinido} & registro(cpf) = FALSE);
  List_Precondition(Machine(MVotacao),concluir_votacao)==(estado = votacao);
  List_Precondition(Machine(MVotacao),apurar_vencedor)==(estado = apuracao)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(MVotacao),apurar_vencedor)==(estado = apuracao | @cc.(cc: candidatos & cc: dom(votos) & ran(votos)/\NAT: FIN(NAT) & votos(cc) = max(ran(votos)) ==> (votos(cc)>SIGMA(ss).(ss: ran(votos) | ss)/2 & !aa.(aa: candidatos & aa/=cc => votos(cc)>votos(aa)) ==> resultado:=cc [] not(votos(cc)>SIGMA(ss).(ss: ran(votos) | ss)/2 & !aa.(aa: candidatos & aa/=cc => votos(cc)>votos(aa))) ==> resultado:=indefinido)));
  Expanded_List_Substitution(Machine(MVotacao),concluir_votacao)==(estado = votacao | estado:=apuracao);
  Expanded_List_Substitution(Machine(MVotacao),votar)==(estado = votacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & zona: ZONAS & cpf: dom(local) & cpf: dom(registro) & zona = local(cpf) & cpf_candidato: CPFs & cpf_candidato: candidatos\/{indefinido} & registro(cpf) = FALSE | cpf_candidato: candidatos & cpf_candidato: dom(votos) & votos(cpf_candidato)+1: NAT ==> votos:=votos<+{cpf_candidato|->votos(cpf_candidato)+1} [] not(cpf_candidato: candidatos & cpf_candidato: dom(votos) & votos(cpf_candidato)+1: NAT) ==> skip || registro:=registro<+{cpf|->TRUE});
  Expanded_List_Substitution(Machine(MVotacao),iniciar_votacao)==(estado = preparacao & candidatos/={} | estado:=votacao);
  Expanded_List_Substitution(Machine(MVotacao),remover_candidato)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & cpf: candidatos | votos,candidatos:={cpf}<<|votos,candidatos-{cpf});
  Expanded_List_Substitution(Machine(MVotacao),adicionar_candidato)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido & cpf/:candidatos | candidatos,votos:=candidatos\/{cpf},votos\/{cpf|->0});
  Expanded_List_Substitution(Machine(MVotacao),mudar_local)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf: dom(local) & zona: ZONAS & zona/=local(cpf) & cpf/=indefinido | local:=local<+{cpf|->zona});
  Expanded_List_Substitution(Machine(MVotacao),remover_eleitor)==(estado = preparacao & cpf: CPFs & cpf: eleitores & cpf/=indefinido | cpf: candidatos ==> votos,candidatos:={cpf}<<|votos,candidatos-{cpf} [] not(cpf: candidatos) ==> skip || local:={cpf}<<|local || registro:={cpf}<<|registro || eleitores:=eleitores-{cpf});
  Expanded_List_Substitution(Machine(MVotacao),adicionar_eleitor)==(estado = preparacao & cpf: CPFs & cpf/:eleitores & zona: ZONAS & cpf/=indefinido | eleitores,local,registro:=eleitores\/{cpf},local\/{cpf|->zona},registro\/{cpf|->FALSE});
  List_Substitution(Machine(MVotacao),adicionar_eleitor)==(eleitores:=eleitores\/{cpf} || local:=local\/{cpf|->zona} || registro:=registro\/{cpf|->FALSE});
  List_Substitution(Machine(MVotacao),remover_eleitor)==(IF cpf: candidatos THEN votos:={cpf}<<|votos || candidatos:=candidatos-{cpf} END || local:={cpf}<<|local || registro:={cpf}<<|registro || eleitores:=eleitores-{cpf});
  List_Substitution(Machine(MVotacao),mudar_local)==(local:=local<+{cpf|->zona});
  List_Substitution(Machine(MVotacao),adicionar_candidato)==(candidatos:=candidatos\/{cpf} || votos:=votos\/{cpf|->0});
  List_Substitution(Machine(MVotacao),remover_candidato)==(votos:={cpf}<<|votos || candidatos:=candidatos-{cpf});
  List_Substitution(Machine(MVotacao),iniciar_votacao)==(estado:=votacao);
  List_Substitution(Machine(MVotacao),votar)==(IF cpf_candidato: candidatos & cpf_candidato: dom(votos) & votos(cpf_candidato)+1: NAT THEN votos:=votos<+{cpf_candidato|->votos(cpf_candidato)+1} END || registro(cpf):=TRUE);
  List_Substitution(Machine(MVotacao),concluir_votacao)==(estado:=apuracao);
  List_Substitution(Machine(MVotacao),apurar_vencedor)==(ANY cc WHERE cc: candidatos & cc: dom(votos) & ran(votos)/\NAT: FIN(NAT) & votos(cc) = max(ran(votos)) THEN IF votos(cc)>SIGMA(ss).(ss: ran(votos) | ss)/2 & !aa.(aa: candidatos & aa/=cc => votos(cc)>votos(aa)) THEN resultado:=cc ELSE resultado:=indefinido END END)
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
  List_ANY_Var(Machine(MVotacao),remover_eleitor)==(?);
  List_ANY_Var(Machine(MVotacao),mudar_local)==(?);
  List_ANY_Var(Machine(MVotacao),adicionar_candidato)==(?);
  List_ANY_Var(Machine(MVotacao),remover_candidato)==(?);
  List_ANY_Var(Machine(MVotacao),iniciar_votacao)==(?);
  List_ANY_Var(Machine(MVotacao),votar)==(?);
  List_ANY_Var(Machine(MVotacao),concluir_votacao)==(?);
  List_ANY_Var(Machine(MVotacao),apurar_vencedor)==(Var(cc) == etype(CPFs,?,?))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(MVotacao)) == (? | ? | registro,votos,local,candidatos,eleitores,estado | ? | adicionar_eleitor,remover_eleitor,mudar_local,adicionar_candidato,remover_candidato,iniciar_votacao,votar,concluir_votacao,apurar_vencedor | ? | seen(Machine(MVotacao_Ctx)) | ? | MVotacao);
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
  Operations(Machine(MVotacao)) == (Type(apurar_vencedor) == Cst(etype(CPFs,?,?),No_type);Type(concluir_votacao) == Cst(No_type,No_type);Type(votar) == Cst(No_type,etype(CPFs,?,?)*etype(ZONAS,?,?)*etype(CPFs,?,?));Type(iniciar_votacao) == Cst(No_type,No_type);Type(remover_candidato) == Cst(No_type,etype(CPFs,?,?));Type(adicionar_candidato) == Cst(No_type,etype(CPFs,?,?));Type(mudar_local) == Cst(No_type,etype(CPFs,?,?)*etype(ZONAS,?,?));Type(remover_eleitor) == Cst(No_type,etype(CPFs,?,?));Type(adicionar_eleitor) == Cst(No_type,etype(CPFs,?,?)*etype(ZONAS,?,?)));
  Observers(Machine(MVotacao)) == (Type(apurar_vencedor) == Cst(etype(CPFs,?,?),No_type))
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
