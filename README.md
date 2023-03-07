# Racket Hero

Jogo de ritmo no estilo Guitar Hero escrito em racket usando a biblioteca 2htdp.

Desenvolvido durante a disciplina de PPLF (6902/02)
no terceiro ano de Ciência da Computação na UEM
para realização do Trbalho Prático 1

**Alunos:**
 - Felipe Gabriel Comin Scheffel - RA117306
 - Douglas Kenji Sakakibara - RA117741

**Professor:** Wagner Igarashi

## Para rodar

Liste as músicas disponíveis com:
```
$ racket racket-hero.rkt -l
```

Inicie o jogo com:
```
$ racket racket-hero.rkt -p <nome-musica>
```

Para iniciar o jogo com a música tocando ao mesmo tempo:
```
$ racket racket-hero.rkt -p <nome-musica> -a <nome-arquivo-audio>
```

Para iniciar o jogo apenas assistindo o computador jogar:
```
$ racket racket-hero.rkt -p <nome-musica> -w
```
## Sincronizando o áudio

O carregamento das notas não está funcionando como o esperado. É possível tentar corrigir isso alterando o valor de `Offset` no chart da música, mas durante o jogo é muito provável que ocorra a desincronização novamente. Consertar a sincronização é o próximo passo do projeto.

## Adicionando músicas

É possível adicionar mais músicas utilizando arquivos de mapeamento desenvolvidos pela comunidade para jogos como o Clone-Hero.

1. Crie uma pasta com o nome da música
2. Adicione o arquivo `notes.chart` e o arquivo de áudio (e.g. `song.mp3`) na pasta
3. Caso o arquivo não se chame `notes.chart`, utilize a flag `-c <nome-chart>`

## TODO

- Corrigir a sincronização de áudio a partir da análise do código do Clone-Hero e similares
- Adicionar medidor de pontuação e multiplicador por combo
- Adicionar suporte as notas com tempo de pressionamento, notas abertas e poder estrela
- Adicionar GUI de menu
