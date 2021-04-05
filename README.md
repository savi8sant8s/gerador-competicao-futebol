# Gerador de competição de futebol

<strong>Construir aplicação:</strong>
```
stack build
```
<strong>Iniciar aplicação:</strong>
```
stack run
```

1. <strong>Gerar competição de eliminatórias</strong> (localhost:8080/v1/gerar-competicao/<strong>mata-mata</strong>):
```
Método: POST

Descrição: Gera os confrontos de uma competição no modelo de mata-mata (eliminatória).
           Aceita opção de jogo único ou casa e fora.
           Aceita opção de sortear times ou não.
           Aceita 4,8,16 ou 32 times. 
           Qualquer quantidade diferente retorna uma lista vazia de partidas.

Entrada: { "times": ["Santa Cruz", "Sport", "Náutico", "Salgueiro"], 
           "temIdaVolta": true, "sortear": true}

Resposta: {
    "tipo": "Eliminatória",
    "partidas": [
        { 
            "fora": ["Náutico", "Sport"],
            "casa": [...],
            "disputa": 1,
            "fase": "Semifinal"
        },
        ...
    ],
    "timestamp": "2021-03-31T12:52:33.111462204Z"
}
```

2. <strong>Gerar competição de fase de grupos</strong> (localhost:8080/v1/gerar-competicao/<strong>fase-grupos</strong>):
```
Método: POST

Descrição: Gera os grupos e suas respectivas partidas de uma competição de fase de grupos.
           Aceita opção de jogo único ou casa e fora.
           Aceita opção de sortear times ou não.
           Aceita 8, 16 ou 32 times. 
           Qualquer quantidade diferente retorna uma lista vazia de partidas.

Entrada: { "times": ["São Paulo", "Corinthians", "Palmeiras", "Internacional", 
                     "Grêmio", "Atl. Mineiro", "Santos", "Ceará"], 
                     "temIdaVolta": false, "sortear": true}

Resposta: {
    "tipo": "Fase de Grupos",
    "segundaFase": [
        "1º do grupo 1 enfrenta 2º do grupo 2",
        "1º do grupo 2 enfrenta 2º do grupo 1"
    ],
    "grupos": [
        {
            "grupo": [ "Palmeiras", "Internacional", "Corinthians", "Santos"],
            "idGrupo": 1,
            "partidas": [
                {
                    "rodada": 1,
                    "partidas": [
                        [ "Palmeiras", "Santos" ], [...]
                    ]
                },
               ...
            ]
        },
       ...
    ],
    "timestamp": "2021-03-31T12:52:33.111462204Z"
}
```

3. <strong>Gerar competição de pontos corridos</strong> (localhost:8080/v1/gerar-competicao/<strong>pontos-corridos</strong>):
```
Método: POST

Descrição: Gera todas as partidas de uma competição de pontos corridos.
           Aceita opção de jogo único ou casa e fora.
           Aceita opção de sortear times ou não.
           Aceita de 2 a 20 times. 
           Qualquer quantidade diferente retorna uma lista vazia de partidas.

Entrada: { "times": ["Real Madrid", "Barcelona", "Atl. de Madrid", "Sevilla"],
           "temIdaVolta": false, "sortear": true}

Resposta: {
    "tipo": "Pontos corridos",
    "partidas": [
        {
            "rodada": 1,
            "partidas": [
                [ "Atl. de Madrid", "Barcelona"], [...]
            ]
        },
       ...
    ],
    "timestamp": "2021-03-31T12:52:33.111462204Z"
}
```

Construído com [Spock](https://www.spock.li/)
