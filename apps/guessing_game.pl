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

% Encontra o atributo mais discriminativo (que divide os personagens de forma mais equilibrada).
best_discriminatory_attribute(Chars, Asked, BestAttr) :-
    get_attributes(Chars, AllAttrs),
    subtract(AllAttrs, Asked, Unasked),
    maplist(attr_discrimination(Chars), Unasked, AttrScores),
    keysort(AttrScores, Sorted),
    last(Sorted, BestAttr-_).  % Pega o atributo com maior poder discriminatório

attr_discrimination(Chars, Attr, Attr-Score) :-
    include(has_attribute(Attr), Chars, With),
    length(Chars, Total),
    length(With, Count),
    Without is Total - Count,
    Score is min(Count, Without).  % Quanto mais balanceado, melhor

% -----------------------------------
% SISTEMA PRINCIPAL DE INICIALIZAÇÃO
% -----------------------------------

% Predicado principal para iniciar o jogo.
startGame :-
    consult('../data/characters.pl'),
    write('Bem-vindo ao Jogo de Identificação de Personagens!'), nl,
    write('Pense em um personagem (real ou fictício) e eu tentarei adivinhar.'), nl,
    write('Responda com "sim." ou "nao." para as características do seu personagem.'), nl, nl,
    prompt_continue,
    !.

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
% - Attempts: tentativas restantes.
% - FilteredCharacters: personagens ainda possíveis.
% - AskedAttributes: atributos já perguntados.

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
    learn_character, !.

% Caso com apenas um personagem possível - adivinhamos.
game(_, [character(Name, _, _, _)], _) :-
    write('Seu personagem é '), write(Name), write('? (sim./nao.) '),
    read(Answer),
    (Answer = sim ->
        write('Acertei!'), nl
    ;
        write('Você venceu! Não consegui adivinhar seu personagem.'), nl,
        learn_character
    ), !.

% Caso geral: faz perguntas.
game(Attempts, RemainingChars, AskedAttrs) :-
    Attempts > 0,
    NewAttempts is Attempts - 1,

%    most_frequent_attribute(RemainingChars, AskedAttrs, Attr, _),
    best_discriminatory_attribute(RemainingChars, AskedAttrs, Attr),

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

ask_question(Attr, Answer) :-
    % Casos específicos para atributos problemáticos.
    (   Attr = 'usa óculos'       	  -> Question = 'Seu personagem usa óculos?'
    ;   Attr = 'usa capa'      	  	  -> Question = 'Seu personagem usa capa?'
    ;   Attr = 'super força'      	  -> Question = 'Seu personagem tem super força?'
    ;   Attr = 'copa do mundo'    	  -> Question = 'Seu personagem tem copa do mundo?'
    ;   Attr = 'prêmio Nobel'     	  -> Question = 'Seu personagem tem prêmio Nobel?'
    ;   Attr = 'fuma cachimbo'    	  -> Question = 'Seu personagem fuma cachimbo?'
    ;   Attr = 'herói'      	  	  -> Question = 'Seu personagem é um herói?'
    ;   Attr = 'cão'      	  	      -> Question = 'Seu personagem é um cão?'
    ;   Attr = 'animal'      	  	  -> Question = 'Seu personagem é um animal?'
    ;   Attr = 'personagem animado'   -> Question = 'Seu personagem é de desenho animado?'
    ;   Attr = 'personagem de filme'  -> Question = 'Seu personagem é de filme?'
    ;   Attr = 'personagem de livro'  -> Question = 'Seu personagem é de livro?'
    ;   Attr = 'personagem de série'  -> Question = 'Seu personagem é de uma série?'

    % Regra padrão para os demais casos.
    ;   format(atom(Question), 'Seu personagem é ~w?', [Attr])
    ),
    write(Question), write('(sim./nao.) '),
    read(Answer).

% Predicado para processar a resposta do usuário sobre um atributo.
process_attribute_answer(sim, Attr, Chars, Asked, NewChars, NewAsked) :-
    include(has_attribute(Attr), Chars, NewChars),
    NewAsked = [Attr|Asked].

process_attribute_answer(nao, Attr, Chars, Asked, NewChars, NewAsked) :-
    exclude(has_attribute(Attr), Chars, NewChars),
    NewAsked = [Attr|Asked].

process_attribute_answer(_, _, Chars, Asked, NewChars, NewAsked) :-
    write('Resposta inválida. Por favor, responda com "sim." ou "nao."'), nl,
    NewChars = Chars,
    NewAsked = Asked.

% ---------------------------------
% APRENDIZADO DE NOVOS PERSONAGENS
% ---------------------------------

learn_character :-
    write('Quero aprender sobre seu personagem.'), nl,
    
    % Nome
    write('Qual é o nome do personagem? (entre aspas e com ponto final)'), nl,
    read(Name),

    % Tipo
    write('Ele é fictício ou real? (ficticio. / real.)'), nl,
    read(Type),

    % Origem
    write('De onde ele é? (entre aspas e com ponto final)'), nl,
    read(Origin),

    % Atributos
    write('Liste os atributos do personagem como uma lista (entre colchetes). Ex: [bruxo, valente, jovem].'), nl,
    read(Attrs),

    % Adiciona dinamicamente e salva no arquivo, se ainda não existir
    (character(Name, Type, Origin, Attrs) ->
        write('Esse personagem já está cadastrado.'), nl
    ;
        Character = character(Name, Type, Origin, Attrs),
        assertz(Character),
        append_character_to_file(Character),
        write('Personagem aprendido com sucesso! Obrigado.'), nl
    ).

append_character_to_file(Character) :- 
    open('../data/characters.pl', append, Stream),
    nl(Stream),
    writeq(Stream, Character),
    write(Stream, '.'),
    close(Stream).

