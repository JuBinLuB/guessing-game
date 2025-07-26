/*
Jogo de identificação de personagens em Prolog.
Autor: Alexssander F. Cândido, Paulo L. Mendes
Data:
*/

% -----------------------------
% BASE DE DADOS DE PERSONAGENS
% -----------------------------

% Base de dados de personagens.
% Cada personagem é representado como character(Nome, Tipo, Origem, [Atributos])
% Onde:
% - Nome: nome do personagem.
% - Tipo: 'ficticio' ou 'real'.
% - Origem: local de origem do personagem.
% - [Atributos]: lista de características adicionais do personagem.

% Personagens fictícios.
character('Harry Potter', ficticio, 'Inglaterra', ['usa óculos', 'bruxo', 'estudante de Hogwarts', 'tem uma cicatriz em forma de raio']).
character('Sherlock Holmes', ficticio, 'Inglaterra', ['detetive', 'usa chapéu', 'fuma cachimbo', 'mora na Baker Street']).
character('Superman', ficticio, 'Krypton', ['super força', 'voa', 'usa capa', 'jornalista']).
character('Mickey Mouse', ficticio, 'Estados Unidos', ['rato', 'usa luvas brancas', 'amigo do Donald']).
character('Gandalf', ficticio, 'Terra Média', ['mago', 'usa chapéu pontudo', 'barba longa', 'carrega um cajado']).
character('Homem-Aranha', ficticio, 'Estados Unidos', ['super herói', 'teia', 'máscara', 'aranha', 'Tio Ben']).
character('Cinderela', ficticio, 'França', ['princesa', 'sapato de cristal', 'abóbora', 'fada madrinha', 'meia-noite']).
character('Shrek', ficticio, 'Pântano', ['ogro', 'verde', 'burro', 'fiona', 'cebola']).
character('Darth Vader', ficticio, 'Tatooine', ['vilão', 'respiração mecânica', 'sabre de luz', 'força', 'pai do Luke']).
character('Barbie', ficticio, 'Estados Unidos', ['boneca', 'loira', 'diversas profissões', 'Ken', 'carro conversível']).

% Personagens reais.
character('Albert Einstein', real, 'Alemanha', ['físico', 'cabelo desgrenhado', 'teoria da relatividade', 'prêmio Nobel']).
character('Cleópatra', real, 'Egito', ['rainha', 'antigo Egito', 'relacionada com Júlio César']).
character('Pelé', real, 'Brasil', ['jogador de futebol', 'rei do futebol', 'três copas do mundo']).
character('Leonardo da Vinci', real, 'Itália', ['pintor', 'inventor', 'Mona Lisa', 'homem vitruviano']).
character('Marilyn Monroe', real, 'Estados Unidos', ['atriz', 'loira', 'cantora', 'ícone pop']).
character('Nelson Mandela', real, 'África do Sul', ['presidente', 'prêmio Nobel', 'apartheid', 'prisão', 'reconciliação']).
character('Steve Jobs', real, 'Estados Unidos', ['Apple', 'iPhone', 'iPad', 'inovação', 'turtleneck']).
character('Madre Teresa', real, 'Índia', ['freira', 'Nobel da Paz', 'calcutá', 'caridade', 'canonizada']).
character('Bruce Lee', real, 'Estados Unidos', ['artista marcial', 'filmes', 'jeet kune do', 'filosofia', 'Enter the Dragon']).
character('Vincent van Gogh', real, 'Holanda', ['pintor', 'orelha cortada', 'girassóis', 'noite estrelada', 'pós-impressionismo']).

% -------------------------------
% PREDICADOS AUXILIARES
% -------------------------------

% Obtém todos os atributos de todos os personagens.
get_attributes(Characters, Attributes) :-
    findall(Attr, 
            (member(character(_, _, _, Attrs), Characters), member(Attr, Attrs)), 
            Attributes).

% Verifica se um personagem tem um determinado atributo.
has_attribute(Attr, character(_, _, _, Attrs)) :- 
    member(Attr, Attrs).

% -----------------------------------
% SISTEMA PRINCIPAL DE INICIALIZAÇÃO
% -----------------------------------

% Predicado principal para iniciar o jogo.
startGame :-
    write('Bem-vindo ao Jogo de Identificação de Personagens!'), nl,
    write('Pense em um personagem (real ou fictício) e eu tentarei adivinhar.'), nl,
    write('Responda com "sim." ou "nao." (incluindo o ponto final) para minhas perguntas.'), nl, nl,
    prompt_continue.

% Confirma se o usuário deseja continuar.
prompt_continue :-
    write('Deseja continuar? (sim./nao.) '),
    read(Answer),
    handle_answer(Answer).

% Caso deseje jogar.
handle_answer(sim) :-
    nl,
    write('Ótimo! Vamos começar...'), nl, nl,
    game(10).

% Caso não deseje jogar.
handle_answer(nao) :-
    nl,
    write('Tudo bem. Até a próxima!'), nl, !.

% Caso a resposta seja inválida.
handle_answer(_) :-
    nl,
    write('Resposta inválida. Por favor, responda com "sim." ou "nao."'), nl, nl,
    prompt_continue.

% --------------------------
% SISTEMA PRINCIPAL DO JOGO
% --------------------------

% Estado do jogo: game(Attempts, FilteredCharacters, AskedAttributes)
% Onde:
% - Attempts: tentativas restantes
% - FilteredCharacters: personagens ainda possíveis
% - AskedAttributes: atributos já perguntados

% Inicia o jogo com estado inicial.
game(Attempts) :-
    ask_type(Type),
    findall(character(Name, Type, Home, Attrs), 
            character(Name, Type, Home, Attrs), 
            FilteredChars),
    game(Attempts, FilteredChars, []).

% Caso base: sem tentativas restantes.
game(0, _, _) :-
    write('Você venceu! Não consegui adivinhar seu personagem.'), nl,
    learn_character.

% Caso com apenas um personagem possível - adivinhamos.
game(_, [character(Name, _, _, _)], _) :-
    write('Seu personagem é '), write(Name), write('? (sim./nao.) '),
    read(Answer),
    (Answer = sim ->
        write('Acertei!'), nl
    ;
        write('Você venceu! Não consegui adivinhar seu personagem.'), nl,
        learn_character
    ).

% Caso geral: faz perguntas.
game(Attempts, RemainingChars, AskedAttrs) :-
    Attempts > 0,
    NewAttempts is Attempts - 1,

    % Obtém todos os atributos possíveis (exceto os já perguntados).
    get_attributes(RemainingChars, AllAttrs),

    % Faz a pergunta.
    ask_question(Attr, Answer),

    % Processa a resposta.
    process_attribute_answer(
        Answer, 
        Attr, 
        RemainingChars, 
        AskedAttrs, 
        NewFiltered, 
        NewAsked),

    % Continua o jogo.
    game(NewAttempts, NewFiltered, NewAsked).

% ---------------------
% SISTEMA DE PERGUNTAS
% ---------------------

% Pergunta se o personagem é fictício ou real.
ask_type(Type) :-
    write('Seu personagem é fictício? (sim./nao.) '),
    read(Answer),
    process_type_answer(Answer, Type).

% Interpreta a resposta sobre o tipo.
process_type_answer(sim, ficticio).
process_type_answer(nao, real).
process_type_answer(_, Type) :-
    write('Resposta inválida. Por favor, responda com "sim." ou "nao."'), nl,
    ask_type(Type).

% Predicado para fazer perguntas ao usuário.
ask_question(Attr, Answer) :-
    write('Seu personagem '), write(Attr), write('? (sim./nao.) '),
    read(Answer).

% Predicado para processar a resposta do usuário sobre um atributo.
process_attribute_answer(sim, Attr, Chars, Asked, NewChars, NewAsked) :-
    write('Ok, seu personagem '), write(Attr), write('.'), nl,
    include(has_attribute(Attr), Chars, NewChars),
    NewAsked = [Attr|Asked].

process_attribute_answer(nao, Attr, Chars, Asked, NewChars, NewAsked) :-
    write('Ok, seu personagem não '), write(Attr), write('.'), nl,
    exclude(has_attribute(Attr), Chars, NewChars),
    NewAsked = [Attr|Asked].

process_attribute_answer(_, _, Chars, Asked, NewChars, NewAsked) :-
    write('Resposta inválida. Por favor, responda com "sim." ou "nao."'), nl,
    NewChars = Chars,
    NewAsked = Asked.

% ---------------------------------
% APRENDIZADO DE NOVOS PERSONAGENS
% ---------------------------------
