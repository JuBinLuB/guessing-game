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

:- dynamic character/4.

% Personagens fictícios.
character('Harry Potter', ficticio, 'Inglaterra', ['bruxo', 'usa óculos', 'protagonista', 'jovem', 'homem', 'europeu', 'estudante', 'personagem de livro', 'personagem de filme']).
character('Peter Parker', ficticio, 'Estados Unidos', ['super herói', 'fotógrafo', 'jovem', 'homem', 'americano', 'nerd', 'tímido', 'protagonista', 'personagem de quadrinho']).
character('Clark Kent', ficticio, 'Estados Unidos', ['super herói', 'usa óculos', 'extraterrestre', 'jornalista', 'voa', 'super força', 'disfarçado', 'homem', 'personagem de quadrinho', 'famoso']).
character('Bruce Wayne', ficticio, 'Estados Unidos', ['super herói', 'rico', 'detetive', 'usa capa', 'humano', 'estrategista', 'homem', 'americano', 'personagem de quadrinho']).
character('Tony Stark', ficticio, 'Estados Unidos', ['engenheiro', 'rico', 'gênio', 'usa armadura', 'inventor', 'sarcastico', 'líder', 'homem', 'americano', 'personagem de quadrinho']).
character('Diana Prince', ficticio, 'Temiscira', ['super herói', 'princesa', 'guerreiro', 'super força', 'imortal', 'mulher', 'personagem de quadrinho', 'justo', 'poderoso']).
character('Homer Simpson', ficticio, 'Springfield', ['pai', 'personagem animado', 'preguiçoso', 'comilão', 'americano', 'homem', 'protagonista', 'personagem de série', 'engraçado']).
character('Elsa', ficticio, 'Arendelle', ['princesa', 'personagem animado', 'poder especial', 'canta', 'mulher', 'loiro', 'protagonista', 'europeu', 'personagem de filme']).
character('Goku', ficticio, 'Terra', ['lutador', 'carismático', 'personagem animado', 'protagonista', 'super força', 'extraterrestre', 'homem', 'personagem de anime', 'corajoso', 'pai', 'leal', 'voa']).
character('Mario', ficticio, 'Reino dos Cogumelos', ['herói', 'personagem de jogo', 'encanador', 'bigode', 'usa boné', 'homem', 'italiano', 'corajoso', 'famoso']).
character('Anakin Skywalker', ficticio, 'Tatooine', ['guerreiro', 'protagonista', 'piloto', 'personagem de filme', 'super força', 'homem', 'jedi', 'famoso']).
character('Yoda', ficticio, 'Desconhecido', ['mentor', 'mestre', 'jedi', 'sábio', 'personagem de filme', 'extraterrestre', 'pequeno', 'verde', 'poderoso']).
character('Sonic', ficticio, 'Mobius', ['personagem de jogo', 'herói', 'velocista', 'animal', 'azul', 'protagonista', 'carismático', 'animado', 'famoso']).
character('Ash Ketchum', ficticio, 'Pallet', ['protagonista', 'personagem animado', 'treinador', 'jovem', 'personagem de anime', 'aventureiro', 'homem', 'determinado', 'explorador', 'amigo leal']).
character('Naruto Uzumaki', ficticio, 'Konoha', ['ninja', 'protagonista', 'carismático', 'personagem animado', 'leal', 'personagem de anime', 'jovem', 'determinado', 'homem', 'loiro', 'guerreiro', 'órfão']).
character('Sailor Moon', ficticio, 'Tóquio', ['personagem animado', 'super herói', 'protagonista', 'personagem de anime', 'estudante', 'líder', 'jovem', 'mulher', 'loiro', 'defensor do bem']).
character('Rick Sanchez', ficticio, 'Estados Unidos', ['cientista', 'inventor', 'gênio', 'personagem de série', 'excêntrico', 'homem', 'viajante', 'crítico', 'anti-herói']).
character('Shrek', ficticio, 'Pântano', ['ogro', 'verde', 'forte', 'mal humorado', 'personagem animado', 'personagem de filme', 'isolado', 'homem', 'protagonista', 'sarcastico']).
character('Mulan', ficticio, 'China', ['guerreiro', 'estrategista', 'protagonista', 'personagem de filme', 'personagem animado', 'corajoso', 'mulher', 'disfarçado', 'asiático', 'patriota']).
character('Aang', ficticio, 'Nômades do Ar', ['personagem animado', 'protagonista', 'monge', 'jovem', 'poder especial', 'homem', 'pacifista', 'careca', 'voa']).
character('Megatron', ficticio, 'Cybertron', ['vilão', 'líder', 'temido', 'robô', 'guerreiro', 'estrategista', 'extraterrestre', 'personagem de filme', 'famoso']).
character('Optimus Prime', ficticio, 'Cybertron', ['herói', 'líder', 'robô', 'honrado', 'estrategista', 'protetor', 'extraterrestre', 'personagem de filme', 'famoso']).
character('Katniss Everdeen', ficticio, 'Panem', ['protagonista', 'arqueiro', 'sobrevivente', 'rebelde', 'mulher', 'jovem', 'líder', 'personagem de livro', 'corajoso']).
character('Frodo Baggins', ficticio, 'Terra Média', ['protagonista', 'aventureiro', 'pequeno', 'herói', 'corajoso', 'humanoide', 'masculino', 'personagem de livro', 'personagem de filme', 'histórico']).
character('Legolas', ficticio, 'Terra Média', ['arqueiro', 'guerreiro', 'preciso', 'elfo', 'leal', 'bonito', 'atlético', 'personagem de filme', 'personagem de livro', 'famoso']).
character('Donatello', ficticio, 'Esgoto', ['tartaruga', 'mutante', 'lutador', 'inteligente', 'cientista', 'ninja', 'herói', 'personagem animado', 'leal', 'inventor']).
character('Michelangelo', ficticio, 'Esgoto', ['tartaruga', 'mutante', 'herói', 'engraçado', 'alegre', 'ninja', 'jovem', 'personagem animado', 'leal', 'comilão']).
character('Raven', ficticio, 'Terra', ['jovem', 'mago', 'personagem de série', 'reservado', 'protetor', 'introvertido', 'mulher', 'poderoso', 'herói']).
character('Eric Cartman', ficticio, 'South Park', ['egoísta', 'personagem animado', 'gordo', 'criança', 'irreverente', 'americano', 'famoso', 'sarcastico', 'provocador']).
character('Wolverine', ficticio, 'Canadá', ['mutante', 'guerreiro', 'homem', 'imortal', 'herói', 'personagem de quadrinho', 'solitário', 'instintivo', 'famoso']).
character('Buzz Lightyear', ficticio, 'Galáxia Infinita', ['herói', 'explorador', 'corajoso', 'personagem animado', 'personagem de filme', 'famoso', 'homem', 'futurista', 'astro espacial', 'confiante']).
character('Woody', ficticio, 'Velho Oeste', ['líder', 'brinquedo', 'personagem animado', 'leal', 'cowboy', 'personagem de filme', 'honesto', 'homem', 'protetor', 'famoso']).
character('Po', ficticio, 'China', ['panda', 'guerreiro', 'desajeitado', 'leal', 'personagem animado', 'personagem de filme', 'herói', 'engraçado', 'determinado']).
character('Zé Pequeno', ficticio, 'Cidade de Deus', ['vilão', 'violento', 'personagem de filme', 'brasileiro', 'jovem', 'negro', 'temido', 'protagonista', 'histórico']).
character('Marinette Dupain-Cheng', ficticio, 'França', ['estudante', 'herói', 'jovem', 'personagem de série', 'personagem animado', 'corajoso', 'mulher', 'francesa', 'poder especial', 'mascarado']).
character('Professor Utonium', ficticio, 'Townsville', ['cientista', 'pai', 'homem', 'personagem animado', 'inventor', 'educador', 'gentil', 'inteligente', 'protetor']).
character('Lisa Simpson', ficticio, 'Springfield', ['inteligente', 'personagem de série', 'estudante', 'criança', 'mulher', 'ativista', 'personagem animado', 'famoso', 'nerd']).
character('Patrick Estrela', ficticio, 'Fenda do Biquíni', ['leal', 'lento', 'engraçado', 'rosado', 'personagem animado', 'preguiçoso', 'marinho', 'masculino']).
character('Bob Esponja', ficticio, 'Fenda do Biquíni', ['amarelo', 'personagem animado', 'trabalhador', 'cozinheiro', 'leal', 'animado', 'herói', 'marinho']).
character('Morty Smith', ficticio, 'Dimensão C-137', ['jovem', 'estudante', 'ingênuo', 'personagem de série', 'personagem animado', 'americano', 'masculino', 'explorador', 'companheiro', 'famoso']).
character('Velma Dinkley', ficticio, 'Estados Unidos', ['inteligente', 'detetive', 'estudante', 'personagem animado', 'usa óculos', 'jovem', 'mulher', 'famoso']).
character('Scooby-Doo', ficticio, 'Estados Unidos', ['animal', 'cão', 'medroso', 'detetive', 'engraçado', 'personagem animado', 'companheiro', 'marrom', 'famoso']).
character('Gru', ficticio, 'Estados Unidos', ['vilão', 'pai', 'careca', 'estrategista', 'personagem animado', 'cientista', 'homem', 'famoso']).
character('Edward Mãos de Tesoura', ficticio, 'Estados Unidos', ['solitário', 'personagem de filme', 'gentil', 'silencioso', 'habilidoso', 'estranho', 'homem']).
character('Luna Lovegood', ficticio, 'Inglaterra', ['estudante', 'personagem de filme', 'bruxo', 'sonhador', 'personagem de livro', 'inteligente', 'mulher', 'corajoso', 'famoso']).
character('Samwise Gamgee', ficticio, 'Terra Média', ['gentil', 'leal', 'personagem de livro', 'personagem de filme', 'jardineiro', 'corajoso', 'protetor', 'herói', 'homem', 'famoso']).
character('Tyrion Lannister', ficticio, 'Westeros', ['pequeno', 'estrategista', 'inteligente', 'sarcastico', 'polêmico', 'nobre', 'personagem de livro', 'personagem de série', 'homem']).
character('Walter White', ficticio, 'Estados Unidos', ['químico', 'professor', 'polêmico', 'pai', 'personagem de série', 'homem', 'criminoso', 'careca']).
character('Hermione Granger', ficticio, 'Inglaterra', ['bruxo', 'inteligente', 'jovem', 'estudante', 'mulher', 'leal', 'personagem de livro', 'personagem de filme', 'europeu']).
character('Professor X', ficticio, 'Estados Unidos', ['mutante', 'telepata', 'líder', 'cadeirante', 'gênio', 'personagem de quadrinho', 'homem', 'mentor']).
character('Gandalf', ficticio, 'Terra Média', ['mago', 'velho', 'barbudo', 'protetor', 'líder', 'estrategista', 'herói', 'sábio']).

% Personagens reais.
character('Albert Einstein', real, 'Alemanha', ['cientista', 'físico', 'gênio', 'prêmio Nobel', 'homem', 'europeu', 'professor', 'pensador', 'conhecido mundialmente']).
character('Marie Curie', real, 'Polônia', ['cientista', 'químico', 'prêmio Nobel', 'pesquisador', 'famoso', 'europeu', 'mulher', 'renomado', 'pioneiro']).
character('Isaac Newton', real, 'Inglaterra', ['cientista', 'físico', 'matemático', 'britânico', 'homem', 'pensador', 'renomado', 'europeu', 'histórico', 'conhecido mundialmente']).
character('Leonardo da Vinci', real, 'Itália', ['artista', 'pintor', 'inventor', 'renascentista', 'europeu', 'homem', 'gênio', 'conhecido mundialmente', 'multitalentoso', 'histórico']).
character('Frida Kahlo', real, 'México', ['artista', 'pintor', 'expressivo', 'latino-americano', 'famoso', 'mulher', 'influente', 'sofrido', 'conhecido mundialmente']).
character('Malala Yousafzai', real, 'Paquistão', ['ativista', 'educador', 'jovem', 'prêmio Nobel', 'mulher', 'asiático', 'corajoso', 'palestrante', 'defensor dos direitos humanos']).
character('Pelé', real, 'Brasil', ['atleta', 'conhecido mundialmente', 'jogador de futebol', 'copa do mundo', 'negro', 'famoso', 'homem', 'ídolo nacional', 'brasileiro', 'histórico']).
character('Usain Bolt', real, 'Jamaica', ['atleta', 'velocista', 'olímpico', 'recordista mundial', 'carismático', 'negro', 'homem', 'jamaicano', 'famoso']).
character('Barack Obama', real, 'Estados Unidos', ['político', 'presidente', 'advogado', 'prêmio Nobel', 'negro', 'americano', 'líder', 'homem', 'influente']).
character('Greta Thunberg', real, 'Suécia', ['ativista', 'clima', 'jovem', 'mulher', 'palestrante', 'europeu', 'famoso', 'autista', 'influente']).
character('Nelson Mandela', real, 'África do Sul', ['político', 'líder', 'ativista', 'preso', 'homem', 'negro', 'africano', 'prêmio Nobel', 'histórico']).
character('Angela Merkel', real, 'Alemanha', ['político', 'líder', 'europeu', 'cientista', 'famoso', 'chanceler', 'renomado', 'mulher', 'influente']).
character('Stephen Hawking', real, 'Inglaterra', ['cientista', 'físico', 'cadeirante', 'pensador', 'homem', 'britânico', 'professor', 'gênio', 'autor']).
character('Beyoncé', real, 'Estados Unidos', ['cantor', 'dançarino', 'famoso', 'mulher', 'ícone pop', 'americano', 'influente']).
character('Vinicius Junior', real, 'Brasil', ['jogador de futebol', 'atleta', 'jovem', 'negro', 'brasileiro', 'homem', 'velocista', 'campeão']).
character('Elon Musk', real, 'África do Sul', ['empresário', 'inventor', 'rico', 'visionário', 'homem', 'africano', 'americano', 'líder', 'famoso']).
character('Ada Lovelace', real, 'Inglaterra', ['matemático', 'programador', 'pioneiro', 'cientista', 'mulher', 'europeu', 'visionário', 'histórico', 'influente']).
character('Gal Gadot', real, 'Israel', ['atriz', 'modelo', 'famoso', 'israelense', 'mulher', 'carismático', 'belo', 'estrela de cinema']).
character('Lionel Messi', real, 'Argentina', ['jogador de futebol', 'camisa 10', 'ídolo nacional', 'campeão', 'atleta', 'copa do mundo', 'argentino', 'conhecido mundialmente', 'homem', 'pequeno', 'canhoto', 'famoso']).
character('Taylor Swift', real, 'Estados Unidos', ['cantor', 'compositor', 'famoso', 'mulher', 'americano', 'popstar', 'loiro', 'influente', 'premiado']).
character('Martin Luther King Jr.', real, 'Estados Unidos', ['líder', 'ativista', 'negro', 'pastor', 'palestrante', 'homem', 'americano', 'histórico', 'prêmio Nobel']).
character('Serena Williams', real, 'Estados Unidos', ['atleta', 'tenista', 'campeão', 'mulher', 'negra', 'americano', 'famoso', 'determinado', 'histórico']).
character('Stephen Curry', real, 'Estados Unidos', ['jogador de basquete', 'atleta', 'armador', 'americano', 'homem', 'campeão', 'líder', 'famoso', 'habilidoso']).
character('Pablo Picasso', real, 'Espanha', ['artista', 'pintor', 'europeu', 'homem', 'inovador', 'histórico', 'revolucionário', 'renomado', 'famoso']).
character('Ayrton Senna', real, 'Brasil', ['piloto', 'brasileiro', 'campeão', 'atleta', 'ídolo nacional', 'famoso', 'homem', 'carismático', 'histórico']).
character('Rihanna', real, 'Barbados', ['cantor', 'empresário', 'mulher', 'negra', 'caribenho', 'famoso', 'estiloso', 'popstar', 'influente', 'conhecido mundialmente']).
character('Chico Xavier', real, 'Brasil', ['médium', 'religioso', 'espiritualista', 'escritor', 'brasileiro', 'homem', 'influente', 'histórico', 'palestrante']).
character('Amelia Earhart', real, 'Estados Unidos', ['piloto', 'explorador', 'mulher', 'americano', 'histórico', 'corajoso', 'pioneiro', 'desaparecido', 'famoso']).
character('JK Rowling', real, 'Inglaterra', ['escritor', 'britânico', 'europeu', 'mulher', 'famoso', 'rico', 'influente', 'renomado', 'conhecido mundialmente']).
character('Albert Sabin', real, 'Estados Unidos', ['cientista', 'médico', 'pesquisador', 'humanitário', 'histórico', 'americano', 'homem', 'conhecido mundialmente', 'influente']).
character('Charles Darwin', real, 'Inglaterra', ['cientista', 'biólogo', 'naturalista', 'pesquisador', 'histórico', 'britânico', 'homem', 'teórico', 'autor']).
character('Madonna', real, 'Estados Unidos', ['cantor', 'dançarino', 'atriz', 'famoso', 'americano', 'popstar', 'mulher', 'controverso', 'ícone pop']).
character('Clarice Lispector', real, 'Brasil', ['escritor', 'filósofo', 'latino-americano', 'mulher', 'influente', 'introspectivo', 'renomado', 'histórico', 'poético']).
character('Michael Jackson', real, 'Estados Unidos', ['cantor', 'dançarino', 'ícone pop', 'americano', 'famoso', 'homem', 'histórico', 'conhecido mundialmente']).
character('Cristiano Ronaldo', real, 'Portugal', ['jogador de futebol', 'europeu', 'portugues', 'campeão', 'ídolo nacional', 'famoso', 'homem', 'atleta', 'histórico', 'conhecido mundialmente']).
character('Marta Vieira da Silva', real, 'Brasil', ['jogador de futebol', 'brasileiro', 'mulher', 'ídolo nacional', 'histórico', 'campeão', 'atleta', 'famoso', 'conhecido mundialmente']).
character('LeBron James', real, 'Estados Unidos', ['jogador de basquete', 'negro', 'campeão', 'homem', 'americano', 'famoso', 'histórico', 'atleta', 'líder']).
character('Guga Kuerten', real, 'Brasil', ['tenista', 'campeão', 'brasileiro', 'carismático', 'histórico', 'homem', 'atleta', 'famoso']).
character('Zinedine Zidane', real, 'França', ['jogador de futebol', 'camisa 10', 'francês', 'europeu', 'técnico', 'campeão', 'homem', 'histórico', 'famoso', 'atleta']).
character('Ronaldinho Gaúcho', real, 'Brasil', ['jogador de futebol', 'habilidoso', 'camisa 10', 'alegre', 'ídolo', 'brasileiro', 'conhecido mundialmente', 'famoso', 'homem', 'campeão', 'atleta']).
character('Neymar Jr.', real, 'Brasil', ['jogador de futebol', 'camisa 10', 'ídolo', 'polêmico', 'atacante', 'famoso', 'homem', 'atleta', 'conhecido mundialmente']).
character('Donald Trump', real, 'Estados Unidos', ['político', 'empresário', 'polêmico', 'presidente', 'homem', 'americano', 'controverso']).
character('Kanye West', real, 'Estados Unidos', ['cantor', 'polêmico', 'empresário', 'homem', 'negro', 'famoso', 'influente']).
character('Aaron Fotheringham', real, 'Estados Unidos', ['atleta', 'cadeirante', 'homem', 'americano', 'pioneiro']).

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

    % Adiciona dinamicamente à base de dados
    assertz(character(Name, Type, Origin, Attrs)),

    write('Personagem aprendido com sucesso! Obrigado.'), nl.