module trabalho1 {
    type Option[a] =
    | Some(a)
    | None

    type TipoCriatura = Mago | Cacador | Druida | Monstro

    type TipoEfeito = Cegueira | Urso | Ovelha

    type Efeito = {
        hp: int,
        tipo_efeito: TipoEfeito
    }

    type Criatura = {
        hp: int,
        classe: TipoCriatura,
        paralisia: bool,
        efeito: Option[Efeito],
        iniciativa: int,
    }

    var criaturas: List[Criatura]
    var criatura: int
    var turno: int
    var info_turno: Option[{ jogador: Criatura, vitima: Option[Criatura], acao_tomada: str, dano: Option[int] }]

    pure def ordena_criaturas(criaturas: List[Criatura]): List[Criatura] =
        criaturas.foldl([], (acc, criatura) => 
            val antes = acc.select(c => criatura.iniciativa < c.iniciativa)
            val depois = acc.select(c => criatura.iniciativa >= c.iniciativa)
            antes.append(criatura).concat(depois)
        )

    pure def ovelha_ou_urso(c: Criatura): bool = 
        match c.efeito {
            | Some(e) => e.tipo_efeito == Urso or e.tipo_efeito == Ovelha
            | None => false
        }

    pure def dano(c: Criatura, d: int): Criatura =
        match c.efeito {
            | Some(e) => if (e.tipo_efeito == Urso or e.tipo_efeito == Ovelha) c.efeito(e.tipo_efeito, e.hp - d) else { ...c, hp: c.hp - d }
            | None => { ...c, hp: c.hp - d }
        }

    pure def efeito(c: Criatura, t: TipoEfeito, hp: int): Criatura = { ...c, efeito: if (hp > 0 or t == Cegueira) Some({ hp: hp, tipo_efeito: t }) else None }

    pure def paralisar(c: Criatura, p: bool): Criatura = { ...c, paralisia: p }

    action ataque(agressor: Criatura): bool = {
        // Escolhendo a vitima com filtro para ajudar o simulador, antes recebia a possivel_vitima escolhida no step mas o monstro nunca morria
        nondet pos_vitima = criaturas.indices().filter(x => x != criatura and if (agressor.classe == Monstro) criaturas[x].classe != Monstro else criaturas[x].classe == Monstro).oneOf()
        val vitima = criaturas[pos_vitima]
        val d = match agressor.efeito {
            | Some(e) => if (agressor.classe == Monstro) 0 else 10
            | None => if (agressor.classe == Monstro and turno != 1) 20 else 10
        }
        all {
            vitima.hp > 0,
            (agressor.classe == Monstro and criaturas.select(x => x.ovelha_ou_urso()).length() > 0)
                implies 
            vitima.ovelha_ou_urso(),
            criaturas' = criaturas.replaceAt(pos_vitima, vitima.dano(d)).replaceAt(criatura, { ...agressor, efeito: None }),
            info_turno' = Some({ jogador: agressor, vitima: Some(vitima), acao_tomada: "ataque", dano: Some(d) })
        }
    }

    action virar_urso(c: Criatura): bool = all {
        c.classe == Druida,
        criaturas' = criaturas.replaceAt(criatura, c.efeito(Urso, 60)),
        info_turno' = Some({ jogador: c, vitima: None, acao_tomada: "virar_urso", dano: None })
    }

    action virar_ovelha(c: Criatura): bool = all {
        c.classe == Mago,
        criaturas' = criaturas.replaceAt(criatura, c.efeito(Ovelha, 1)),
        info_turno' = Some({ jogador: c, vitima: None, acao_tomada: "virar_ovelha", dano: None })
    }

    action cegar_monstro(personagem: Criatura, pos_monstro: int): bool = {
        val monstro = criaturas[pos_monstro]
        all {
            monstro.hp > 0,
            personagem.classe == Cacador,
            monstro.classe == Monstro,
            monstro.efeito == None,
            criaturas' = criaturas.replaceAt(pos_monstro, monstro.efeito(Cegueira, 0)).replaceAt(criatura, { ...personagem, efeito: None }),
            info_turno' = Some({ jogador: personagem, vitima: Some(monstro), acao_tomada: "cegar_monstro", dano: None })
        }
    }

    action paralisar_personagem(monstro: Criatura, pos_personagem: int): bool = {
        val personagem = criaturas[pos_personagem]
        all {
            personagem.hp > 0,
            not(personagem.paralisia),
            monstro.classe == Monstro,
            personagem.classe != Monstro,
            criaturas' = criaturas.replaceAt(pos_personagem, personagem.paralisar(true)).replaceAt(criatura, { ...monstro, efeito: None }),
            info_turno' = Some({ jogador: monstro, vitima: Some(personagem), acao_tomada: "paralisar_personagem", dano: None })
        }
    }

    action remover_paralisia(personagem: Criatura, pos_personagem_paralisado: int): bool = {
        val personagem_paralisado = criaturas[pos_personagem_paralisado]
        all {
            personagem_paralisado.hp > 0,
            personagem_paralisado.paralisia,
            personagem_paralisado.classe != Monstro,
            personagem.classe != Monstro,
            personagem != personagem_paralisado,
            criaturas' = criaturas.replaceAt(pos_personagem_paralisado, personagem_paralisado.paralisar(false)).replaceAt(criatura, { ...personagem, efeito: None }),
            info_turno' = Some({ jogador: personagem, vitima: Some(personagem_paralisado), acao_tomada: "remover_paralisia", dano: None })
        }
    }

    action init = {
        val dado = 1.to(20)
        nondet ini_mago = dado.oneOf()
        nondet ini_cacador = dado.oneOf()
        nondet ini_druida = dado.oneOf()
        nondet ini_monstro = dado.oneOf()
        all {
            criaturas' = ordena_criaturas([{ hp: 20, classe: Mago, paralisia: false, efeito: None, iniciativa: ini_mago },
                { hp: 20, classe: Cacador, paralisia: false, efeito: None, iniciativa: ini_cacador},
                { hp: 20, classe: Druida, paralisia: false, efeito: None, iniciativa: ini_druida},
                { hp: 100, classe: Monstro, paralisia: false, efeito: None, iniciativa: ini_monstro}]),
            criatura' = 0,
            turno' = 1,
            info_turno' = None
        }
    }

    action step = {
        val jogador = criaturas[criatura]
        nondet possivel_vitima = criaturas.indices().oneOf()
        all {
            jogador.hp > 0,
            not(jogador.paralisia),
            any {
                paralisar_personagem(jogador, possivel_vitima),
                remover_paralisia(jogador, possivel_vitima),
                cegar_monstro(jogador, possivel_vitima),
                jogador.virar_ovelha(),
                jogador.virar_urso(),
                jogador.ataque()
            },
            criatura' = if (criatura + 1 == criaturas.length()) 0 else criatura + 1,
            turno' = if (criatura + 1 == criaturas.length()) turno + 1 else turno,
        }
    }

    val monstro_nao_morre = criaturas.indices().filter(i => criaturas[i].classe == Monstro).forall(i => criaturas[i].hp > 0)
    val personagens_nao_morrem = criaturas.indices().filter(i => criaturas[i].classe != Monstro).forall(x => criaturas[x].hp > 0)
}