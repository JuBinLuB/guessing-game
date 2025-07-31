# Jogo de Adivinhação (Prolog)

Um jogo de adivinhação implementado em Prolog, onde o sistema tenta descobrir um personagem com base em perguntas de sim/não. Se falhar, ele aprende o novo personagem e persiste esse conhecimento para futuras sessões.

## Funcionalidades

- Advinhação interativa: O jogo faz perguntas de sim/não para inferir qual personagem o jogador está pensando.
- Base de conhecimento dinâmica: O sistema aprende novos personagens e seus atributos se não consegue advinhar, expandindo sua base de dados para futuras jogadas.
- Seleção de perguntas inteligente: Utiliza um algoritmo que seleciona atributos com alto poder discriminatório.
- Persistência de dados: Os personagens aprendidos são salvos em `data/characters.pl` para manter o conhecimento entre execuções.

## Estrutura de Pastas

```
/
├── apps/
│   └── guessing_game.pl       % Lógica principal do jogo
├── data/
│   └── characters.pl          % Base de dados de personagens persistida
├── docs/
│   └── TPProlog.pdf           % Documentação do projeto
```

## Como Executar

1. Certifique-se de ter o SWI-Prolog instalado. 

2. Clone os arquivos do projeto para seu ambiente local.

3. Em seguida, no diretório `apps/`:

```bash
swipl guessing_game.pl
```

4. Dentro do interpretador Prolog, inicie o jogo com:

```prolog
startGame.
```
Siga as instruções na tela para jogar. O sistema fará perguntas e, se não souber um personagem, pedirá para você adicioná-lo à base de conhecimento.

## Observações

- As respostas devem ser `sim` ou `nao`.
