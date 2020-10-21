*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : FUNCOES.PRG
* Data        : 01/01/99
* Objetivo    : PROGRAMA DE FUNCOES DO SISTEMA
* PRG"s       : MENU
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION MOLDURA(topo,esq,dir ,baixo,duplo,texto)
IF PCOUNT()<6
   @ 23,10 SAY "NUMERO DE PARAMETROS INVALIDOS"
   RETURN nil (.F.)
ENDIF
IF DUPLO
   @ topo,esq TO DIR ,baixo double
ELSE
   @ topo,esq TO DIR ,baixo
ENDIF
IF LEN(texto)>0
   tam=baixo-esq
   novapos=(tam-LEN(texto))/2
   @ topo,esq+novapos SAY texto
ENDIF
RETURN nil (.T.)
***

FUNCTION CALCULA()
SET CURSOR OFF
SAVE SCREEN TO TELA_CALC
SET COLOR TO GR+/B,W+/N,,,W/B
@  1,52 SAY "ááááááááááááááááááááááááÕª"
@  2,52 SAY "á                        á"
@  3,52 SAY "ááááááááááááááááááááááááÕπ"
@  4,52 SAY "áááááááááááááááááááááááááá"
@  5,52 SAY "áááááááááááááááááááƒøááááá"
@  6,52 SAY "ááááá % á / á * á - áááááá"
@  7,52 SAY "áááááááááááááááááááƒ¥ááááá"
@  8,52 SAY "ááááá 7 á 8 á 9 á   áááááá"
@  9,52 SAY "áááááááááááááááƒ¥ + áááááá"
@ 10,52 SAY "ááááá 4 á 5 á 6 á   áááááá"
@ 11,52 SAY "áááááááááááááááááááƒ¥ááááá"
@ 12,52 SAY "ááááá 1 á 2 á 3 á   áááááá"
@ 13,52 SAY "áááááááááááááááƒ¥ = áááááá"
@ 14,52 SAY "ááááá   0   á . á   áááááá"
@ 15,52 SAY "ááááááááááááááááááááŸ±áááá"
@ 16,52 SAY "ááCalculadora Eletronicaáá"
@ 17,52 SAY "áááááááááááááááááááááááááá"
@ 18,52 SAY "ááááááááááááááááááááááááÕπ"
@ 19,52 SAY "á Esc - Sair - C - Limpa á"
@ 20,52 SAY "ááááááááááááááááááááááááÕπ"
@ 21,52 SAY "á                        á"
@ 22,52 SAY "á                        á"
@ 23,52 SAY "ááááááááááááááááááááááááÕº"
SET COLOR TO GR+/N
SET COLOR TO B+/W
@ 21,53 SAY "       Ultima Aá∆o      "
@ 22,53 SAY "                        "
SET COLOR TO
DECLARE vNUMERO[500],vMEMORIA[500]
AFILL(vNUMERO,0)
AFILL(vMEMORIA,0)
LIMPA    = .F.
TECL_ATU = 0
TECL_ANT = 0
eAUX_N   = 1
eAUX_M   = 1
DO WHILE .T.
   * Visor Atual
   CLEAR GETS
   @  2,54 GET vNUMERO[eAUX_N]  PICT "@R 999,999,999,999,999.99"
   eAUX_N = eAUX_N + 1
   READ
   TECL_ATU  = 0
   TECL_ATU  = INKEY(0)
   DO CASE
      CASE TECL_ATU = 27
         * Sair da Calculadora
         SET CURSOR ON
         CLEAR
         RESTORE SCREEN FROM TELA_CALC
         SET COLOR TO W+/B
         SET COLOR TO
         RETURN nil
      CASE TECL_ATU = ASC("M") .OR. TECL_ATU = ASC("m")
         * Gravar na Memoria
         SET COLOR TO R/N
         @ 22,67 SAY " M "
         TONE(4000,1)
         SET COLOR TO GR+/N
         @ 22,67 SAY " M "
         SET COLOR TO
         vMEMORIA[eAUX_M] = vNUMERO[eAUX_N]
         LOOP
      CASE TECL_ATU = ASC("R") .OR. TECL_ATU = ASC("r")
         * Restaurar da Memoria
         SET COLOR TO R/N
         @ 22,71 SAY " R "
         TONE(4000,1)
         SET COLOR TO GR+/N
         @ 22,71 SAY " R "
         SET COLOR TO
         vNUMERO[eAUX_N] = vMEMORIA[eAUX_M]
         LOOP
      CASE TECL_ATU = ASC("C") .OR. TECL_ATU = ASC("c")
         * Limpar Calculos
         AFILL(vNUMERO,0)
         SET COLOR TO R/N
         @ 22,63 SAY " C "
         TONE(4000,1)
         SET COLOR TO GR+/N
         @ 22,63 SAY " C "
         SET COLOR TO
         LOOP
      CASE TECL_ATU = ASC("*")
         SET COLOR TO R/N
         @ 6,65 SAY " * "
         TONE(4000,1)
         SET COLOR TO GR+/B
         @ 6,65 SAY " * "
         SET COLOR TO
*         eAUX_N   = eAUX_N + 1
         TECL_ANT = TECL_ATU
      CASE TECL_ATU = ASC("+")
         SET COLOR TO R/N
         @ 8,69 SAY  "   "
         @ 9,69 SAY  " + "
         @ 10,69 SAY "   "
         TONE(4000,1)
         SET COLOR TO GR+/B
         @ 8,69 SAY  "   "
         @ 9,69 SAY  " + "
         @ 10,69 SAY "   "
         SET COLOR TO
*        eAUX_N   = eAUX_N + 1
         TECL_ANT = TECL_ATU
      CASE TECL_ATU = ASC("=")
         SET COLOR TO R/N
         @ 12,69 SAY "   "
         @ 13,69 SAY " = "
         @ 14,69 SAY "   "
         TONE(4000,1)
         SET COLOR TO GR+/B
         @ 12,69 SAY "   "
         @ 13,69 SAY " = "
         @ 14,69 SAY "   "
         SET COLOR TO
*         eAUX_N   = eAUX_N + 1
*         TECL_ANT = TECL_ATU
      CASE TECL_ATU = ASC("-")
         SET COLOR TO R/N
         @ 6,69 SAY " - "
         TONE(4000,1)
         SET COLOR TO GR+/B
         @ 6,69 SAY " - "
         SET COLOR TO
*         eAUX_N   = eAUX_N + 1
         TECL_ANT = TECL_ATU
      CASE TECL_ATU = ASC("/")
         SET COLOR TO R/N
         @ 6,61 SAY " / "
         TONE(4000,1)
         SET COLOR TO GR+/B
         @ 6,61 SAY " / "
         SET COLOR TO
*         eAUX_N   = eAUX_N + 1
         TECL_ANT = TECL_ATU
      CASE TECL_ATU = ASC("%") 
         SET COLOR TO R/N
         @ 6,57 SAY " % "
         TONE(4000,1)
         SET COLOR TO GR+/B
         @ 6,57 SAY " % "
         SET COLOR TO
*         eAUX_N   = eAUX_N + 1
         TECL_ANT = TECL_ATU
   ENDCASE   
   IF TECL_ATU = ASC("=") 
      DO CASE
         CASE TECL_ANT = ASC("*")
            eAUX_N   = eAUX_N + 1
            vNUMERO[eAUX_N] = vNUMERO[eAUX_N - 3] * vNUMERO[eAUX_N - 2]
            @  2,54 GET vNUMERO[eAUX_N]  PICT "@R 999,999,999,999,999.99"
            CLEAR GETS
            TECL_ANT = 0                           
         CASE TECL_ANT = ASC("+")
            eAUX_N   = eAUX_N + 1
            vNUMERO[eAUX_N] = vNUMERO[eAUX_N - 3] + vNUMERO[eAUX_N - 2]
            @  2,54 GET vNUMERO[eAUX_N]  PICT "@R 999,999,999,999,999.99"
            CLEAR GETS
            TECL_ANT = 0                           
         CASE TECL_ANT = ASC("-")
            eAUX_N   = eAUX_N + 1
            vNUMERO[eAUX_N] = vNUMERO[eAUX_N - 3] - vNUMERO[eAUX_N - 2]
            @  2,54 GET vNUMERO[eAUX_N]  PICT "@R 999,999,999,999,999.99"
            CLEAR GETS
            TECL_ANT = 0                           
         CASE TECL_ANT = ASC("/")
            eAUX_N   = eAUX_N + 1
            vNUMERO[eAUX_N] = vNUMERO[eAUX_N - 3] / vNUMERO[eAUX_N - 2]
            @  2,54 GET vNUMERO[eAUX_N]  PICT "@R 999,999,999,999,999.99"
            CLEAR GETS
            TECL_ANT = 0                           
         CASE TECL_ANT = ASC("%")
            eAUX_N   = eAUX_N + 1
            vNUMERO[eAUX_N] = vNUMERO[eAUX_N - 3] * (vNUMERO[eAUX_N - 2]/100)
            @  2,54 GET vNUMERO[eAUX_N]  PICT "@R 999,999,999,999,999.99"
            CLEAR GETS
            TECL_ANT = 0
      ENDCASE
   ENDIF
ENDDO
SET CURSOR ON
CLEAR
RESTORE SCREEN FROM TELA_CALC
SET COLOR TO
SET COLOR TO W/N,+W/BG
RETURN nil
********************************** fim ************************************
***************************************************************************
*** Funcao Bloco de Notas
FUNCTION NOTAS
SET CURSOR ON
BOXnot1=repl(CHR(219),8)+CHR(176)
BOXnot2=CHR(201)+CHR(205)+CHR(187)+CHR(186)+;
     CHR(188)+CHR(205)+CHR(200)+CHR(186)+CHR(32)
save screen to t_not_in

@ 1,0,24,79 BOX BOXnot1

DECLARE arqtex[adir("*.TXT")]
ADIR("*.TXT",arqtex)
IF LEN(arqTEX)=0 .OR. TYPE("ARQTEX[1]")="U"
  OK_NOTA=CONFIRMA ("NAO EXITEM ARQUIVOS TEXTOS NESSA DIRETORIA CRIA UM NOVO ?")
  IF OK_NOTA=2  && NEGATIVE
     RESTORE SCREEN FROM T_NOT_IN
     RETURN nil
   ELSE
     ARQ_NOVO=SPACE(8)
     @ 3,3,5,44 BOX BOXNOT2
     CLEAR GETS
     @ 4,4 SAY "DIGITE O NOME DO ARQUIVO--> "
     @ 4,30 GET ARQ_NOVO PICT "@!")
     READ
     ARQ_NOVO=ALLTRIM(ARQ_NOVO)+".TXT"
     @ 4,30 SAY ARQ_NOVO
     MOLDURA(9,4,21,76,.F.,"&ARQ_NOVO")
     TEXTO=SPACE(1)
     TEXTO=MEMOEDIT(TEXTO,10,5,20,75,.T.)
     OK_NOTA=CONFIRMA("CONFIRMA A GRAVACAO DO ARQUIVO  &ARQ_NOVO. ")
       IF OK_NOTA=1
        MEMOWRIT("&ARQ_NOVO.",TEXTO)
       ENDIF
     RESTORE SCREEN FROM t_NOT_IN
  ENDIF
ELSE
  DO WHILE .T.
   @ 2,2 CLEAR TO 10,15
   MOLDURA(2,2,10,15,.F.,"ARQUIVOS")
   opc=ACHOICE(3,3,9,14,arqTEX)
   IF opc=0
     ok_nota=confirma("ABANDONA (S) ou EDITA um novo (N)")
     IF ok_nota=1  && positive
        RESTORE SCREEN FROM T_NOT_IN
        EXIT
     ENDIF
     IF OK_NOTA=2
       ARQ_NOVO=SPACE(8)
       @ 3,3,5,44 BOX BOXNOT2
       CLEAR GETS
       @ 4,4 SAY "DIGITE O NOME DO ARQUIVO--> "
       @ 4,32 GET ARQ_NOVO PICT "@!")
       READ
       ARQ_NOVO=TRIM(ARQ_NOVO)+".TXT"
       @ 4,32 SAY ARQ_NOVO
       MOLDURA(9,4,21,76,.F.,"&ARQ_NOVO.")
       TEXTO=SPACE(3)
       TEXTO=MEMOEDIT(TEXTO,10,5,20,75,.T.)
       OK_NOTA=CONFIRMA("CONFIRMA A GRAVACAO DO ARQUIVO  &ARQ_NOVO. ")
       IF OK_NOTA=1
          MEMOWRIT("&ARQ_NOVO.",TEXTO)
       ENDIF
       RESTORE SCREEN FROM t_NOT_IN
       RETURN nil
     ENDIF
   ELSE
      arq_NOVO=arqTEX[opc]
      MOLDURA(9,4,21,76,.F.,"&ARQ_NOVO.")
      TEXTO=MEMOREAD("&ARQ_NOVO.")
      TEXTO=MEMOEDIT(TEXTO,10,5,20,75,.T.)
      OK_NOTA=CONFIRMA("CONFIRMA A GRAVACAO DO ARQUIVO  &ARQ_NOVO. ")
      IF OK_NOTA=1
      MEMOWRIT("&ARQ_NOVO.",TEXTO)
      RESTORE SCREEN FROM t_NOT_IN
    ENDIF
  ENDIF
  ENDDO
ENDIF
RETURN nil

FUNCTION CONFIRMA(texto)
largjan=LEN(texto)+2
IF largjan<16
   largjan=16
ENDIF
CES=(80-LARGJAN)/2
LES=19
telavelha=SAVESCREEN(les,0,les+4,79)
corantig=SETCOLOR()
cornova="W/B,N/W"
SET COLOR TO &CORNOVA
@ les,ces CLEAR TO les+3,ces+largjan
@ les,ces TO les+3,ces+largjan double
@ les+1,ces+2  SAY texto
coluna=(largjan-9)/2
@ les+2,ces+coluna  PROMPT "Sim"
@ les+2,ces+coluna+6 PROMPT "Nao"
MENU TO CONFIRM
SET COLOR TO &CORANTIG
RESTSCREEN(les,0,les+4,79,telavelha)
RETURN nil (CONFIRM )

********************************** fim ************************************
***************************************************************************
