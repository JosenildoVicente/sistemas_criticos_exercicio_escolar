/* MVotacao
 * Author: Josenildo Vicente
 * Creation date: 26/04/2021
 */
MACHINE
    MVotacao
    
SEES
    MVotacao_Ctx
    
ABSTRACT_VARIABLES
    estado
    ,eleitores
    ,candidatos
    ,local
    ,votos
    ,registro
    
INVARIANT
    estado : ESTADO
    & eleitores : FIN(CPFs)
    & candidatos : FIN(CPFs)
    & local : eleitores +-> ZONAS
    & votos : candidatos +-> NAT
    & registro : eleitores +-> BOOL
    & ! aa . ( aa : eleitores => aa /= indefinido )
    & ! ee . ( ee : candidatos => ee /= indefinido )
    & ! ii . ( ii : candidatos => ii : eleitores )
    & ! oo . ( oo : ran(registro) & estado = preparacao => oo = FALSE )
    & ! uu . ( uu : ran(votos) & estado = preparacao => uu = 0 )

INITIALISATION
    estado := preparacao
    || eleitores := {}
    || candidatos := {}
    || local := {}
    || votos := {}
    || registro := {} 
    
OPERATIONS
    
    adicionar_eleitor ( cpf, zona ) =
    PRE
        estado = preparacao
        & cpf : CPFs
        & cpf /: eleitores
        & zona : ZONAS
        & cpf /= indefinido
    THEN
       eleitores := eleitores \/ { cpf }
       || local := local \/ { cpf |-> zona }
       || registro := registro \/ { cpf |-> FALSE }
    END
    ;
    
    remover_eleitor ( cpf ) =
    PRE
        estado = preparacao
        & cpf : CPFs
        & cpf : eleitores
        & cpf /= indefinido
    THEN
        IF cpf : candidatos THEN votos := { cpf } <<| votos  || candidatos := candidatos - { cpf } END
        || local := { cpf } <<| local
        || registro := { cpf } <<| registro
        || eleitores := eleitores - { cpf }
    END
    ;
    
    mudar_local ( cpf, zona ) =
    PRE
        estado = preparacao
        & cpf : CPFs
        & cpf : eleitores
        & cpf : dom(local)
        & zona : ZONAS
        & zona /= local(cpf)
        & cpf /= indefinido
    THEN
        local:= local <+ {cpf |-> zona}
    END
    ;
    
    adicionar_candidato ( cpf ) =
    PRE
        estado = preparacao
        & cpf : CPFs
        & cpf : eleitores
        & cpf /= indefinido
        & cpf /: candidatos
    THEN
       candidatos := candidatos \/ { cpf }
       || votos := votos \/ { cpf |-> 0 }
    END
    ;
    
    remover_candidato ( cpf ) =
    PRE
        estado = preparacao
        & cpf : CPFs
        & cpf : eleitores
        & cpf /= indefinido
        & cpf : candidatos
    THEN
       votos := { cpf } <<| votos
       || candidatos := candidatos - { cpf }
    END
    ;
    
    iniciar_votacao =
    PRE
        estado = preparacao
        & candidatos /= {}
    THEN
       estado := votacao
    END
    ;
    
    votar ( cpf, zona, cpf_candidato)=
    PRE
        estado = votacao
        & cpf : CPFs
        & cpf : eleitores
        & cpf /= indefinido
        & zona : ZONAS
        & cpf : dom(local)
        & cpf : dom(registro)
        & zona = local(cpf)
        & cpf_candidato : CPFs
        & (cpf_candidato : (candidatos \/ { indefinido} ))
        & registro(cpf) = FALSE
    THEN
        IF cpf_candidato : candidatos & cpf_candidato : dom(votos) & (votos(cpf_candidato) + 1) : NAT
        THEN
            votos := votos <+ { cpf_candidato |-> ( votos(cpf_candidato) + 1 ) }
        END
        || registro(cpf) := TRUE
    END
    ;
    
    concluir_votacao =
    PRE
        estado = votacao
    THEN
       estado := apuracao
    END
    ;
    
    
    resultado <-- apurar_vencedor=
    PRE
        estado = apuracao
    THEN
        ANY cc 
        WHERE  
            cc : candidatos
            & cc : dom(votos)
            & ran(votos) /\ NAT : FIN(NAT)
            & votos(cc) = max(ran(votos))
        THEN
            IF (votos(cc) > (SIGMA ss . (ss : ran(votos) | ss)) / 2 ) & ( ! aa . ( aa : candidatos & aa /= cc => votos(cc) > votos(aa)))
            THEN
               resultado := cc
            ELSE
                resultado := indefinido
            END
        END
            
    END

END

/*

                        Obrigações de provas não provadas automáticamente:

- remover_eleitor.9
    cpf : candidatos & btrue & oo : ran({cpf} <<| registro) & estado = preparacao  => oo = FALSE
justificativa: 
    Ele não consegue provar que o registro do eleitor fica falso quando ele é removido, só que não fica falso, está sendo excluido o registro, logo não fica nem falso nem verdadeiro.

- remover_eleitor.16
    not(cpf : candidatos) & btrue & oo : ran({cpf} <<| registro) & estado = preparacao  => oo = FALSE
justificativa:
    Mesma situação da obrigação acima: Não consegue provar que o registro do eleitor fica falso quando ele é removido, só que não fica falso, está sendo excluido o registro, logo não fica nem falso nem verdadeiro.

- remover_eleitor.10
cpf : candidatos &
    btrue & uu : ran({cpf} <<| votos) & estado = preparacao  => uu = 0
justificativa:
    Ele não consegue provar que os votos do eleitor que é candidato fica zerado quando ele é removido, só que não fica zerado, está sendo excluido o cpf de votos, logo não fica zerado nem com nenhum outro valor.

- remover_candidato.5
    btrue & uu : ran({cpf} <<| votos) & estado = preparacao  => uu = 0
justificativa:
     Mesmo caso a obrigação acima em que: Ele não consegue provar que os votos do candidato fica zerado quando ele é removido, só que não fica zerado, está sendo excluido o cpf de votos, logo não fica zerado nem com nenhum outro valor.
     
*/
