*********************************************************************
* Sistema     : GestÆo de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : ALTERA.PRG
* Data        : 01/01/1999
* Objetivo    : ALTERACAO DE CLIENTES
* PRG's       : ---
* DBF's       : ---
* Alteracao   :
*********************************************************************
SAVE SCREEN TO TELA03
DO PTIT WITH 'ALTERACAO DE CLIENTES'
SELE 1
USE STATUS
GOTO TOP
BLOCO_CLI = BLOCO_CLIE                    && Bloco de Notas do Cliente
SELE 1
USE CONTATO INDEX CONTATO,NUM_CLIE
GO TOP
DO WHILE .T.
   DO PLIMPA
   SET COLOR TO W/N,+W/R
   ePESQUISA = SPACE(49)
   @ 05,02 SAY 'Cliente     :'  GET ePESQUISA PICT '@! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   READ
   ePESQUISA = ALLTRIM(ePESQUISA)
   SEEK ePESQUISA
   eNUM_CLIE = NUM_CLIE
   DECLARE CAMPO[1],PIC[1],CAB[1],LENS[1]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]='@S50'
   CAB[1]='  NOME'
   DBEDIT(5,15,5,65,CAMPO,'KEYS_3',PIC,CAB,' ',' ',' ')
   IF F_BLOQR()
      @ 07,02 SAY 'CPF ou CGC    : ' GET CPF_CGC
      @ 08,02 SAY 'RG            : ' GET RG
      @ 08,44 SAY 'UF : '            GET UF_RG PICT '@!XX'
      @ 09,02 SAY 'Emissƒo       : ' GET EMISSAO_RG
      @ 09,41 SAY 'Orgƒo : '         GET ORGAO_RG
      @ 10,02 SAY 'Sexo (M/F/O)  : ' GET SEXO PICT 'X' VALID SEXO $('MFO')
      @ 10,36 SAY 'Nascimento : '    GET NASCIMENTO
      READ
      eCPF_CGC = CPF_CGC
      IF SEXO = 'M' .OR. SEXO = 'F'
         IF .NOT. TESTACPF(eCPF_CGC)
            DO PLQUA WITH '01'
            DO PMSGR WITH 'Aten‡Æo !!!',3
            DO PMENSAG WITH 'CPF nao confere !'
            DO PPARA
         ENDIF
      ELSE
         IF .NOT. TESTACGC(eCPF_CGC)
            DO PLQUA WITH '01'
            DO PMSGR WITH 'Aten‡Æo !!!',3
            DO PMENSAG WITH 'CGC nao confere !'
            DO PPARA
         ENDIF
      ENDIF
      SET COLOR TO W/N+
      @ 12,02 SAY 'ENDERECO RESIDENCIAL'
      SET COLOR TO W/N,+W/R
      * TERCEIRA PAGINA
      @ 14,02 SAY 'Endere‡o      : ' GET END_R
      @ 15,02 SAY 'Bairro        : ' GET BAIRRO_R
      @ 16,02 SAY 'Cidade        : ' GET CIDADE_R
      @ 17,02 SAY 'Estado        : ' GET ESTADO_R PICT '@!XX'
      @ 18,02 SAY 'CEP           : ' GET CEP_R   PICT '@R 99999-999'
      @ 19,02 SAY 'Telefone  # 1 : ' GET TEL_R_1 PICT '@R (XX) XXXXX-XXXXX'
      @ 20,02 SAY 'Telefone  # 2 : ' GET TEL_R_2 PICT '@R (XX) XXXXX-XXXXX'
      @ 21,02 SAY 'e-mail        : ' GET MAIL_PROP
      READ
      DO PMENSAG WITH 'Pressione Qualquer Tecla Para Continuar ...'
      DO PPARA
      DO PLQUA WITH '01'
      @ 07,01 CLEAR TO 21,79
      SET COLOR TO W/N+
      @ 07,02 SAY 'ENDERECO COMERCIAL'
      SET COLOR TO W/N,+W/R
      * QUARTA PAGINA
      @ 09,02 SAY 'Empresa       : ' GET EMPRESA
      @ 10,02 SAY 'Profissao     : ' GET PROFISSAO
      @ 11,02 SAY 'Endere‡o      : ' GET END_C
      @ 12,02 SAY 'Bairro        : ' GET BAIRRO_C
      @ 13,02 SAY 'Cidade        : ' GET CIDADE_C
      @ 14,02 SAY 'Estado        : ' GET ESTADO_C PICT '@!XX'
      @ 15,02 SAY 'CEP           : ' GET CEP_C   PICT '@R 99999-999'
      @ 16,02 SAY 'Telefone  # 1 : ' GET TEL_C_1 PICT '@R (XX) XXXXX-XXXXX - (XXXXXXXXXXXXXXXXXXXX)'
      @ 17,02 SAY 'Telefone  # 2 : ' GET TEL_C_2 PICT '@R (XX) XXXXX-XXXXX - (XXXXXXXXXXXXXXXXXXXX)'
      @ 18,02 SAY 'E-Mail        : ' GET MAIL_C        
      @ 20,02 SAY 'Correspondencia na Residencia ou Escritorio (R/E) ? ' GET OPCAO PICT '@!X' VALID OPCAO $('RE')
      READ
      DO PMENSAG WITH 'Pressione Qualquer Tecla Para Continuar ...'
      DO PPARA
      @ 07,01 CLEAR TO 21,79
      SET COLOR TO W/N+
      @ 07,02 SAY 'OBSERVACOES GERAIS'
      SET COLOR TO W/N,+W/R
      * QUINTA PAGINA
      @ 08,00 GET OBSERVA_1
      @ 09,00 GET OBSERVA_2
      @ 10,00 GET OBSERVA_3
      @ 11,00 GET OBSERVA_4
      @ 12,00 GET OBSERVA_5
      READ
      SET COLOR TO W/N+
      @ 14,02 SAY 'DADOS DO CONJUGE'
      SET COLOR TO W/N,+W/R
      @ 15,02 SAY 'Nome Conjuge  : ' GET CONJUGE
      @ 16,02 SAY 'Nascimento    : ' GET NASC_CONJ
      @ 17,02 SAY 'CPF Conjuge   : ' GET CPF_CONJ
      @ 18,02 SAY 'Telefone Conj : ' GET TEL_CONJ PICT '@R (XX) XXXXX-XXXXX - (XXXXXXXXXXXXXXXXXXXX)'
      @ 19,02 SAY 'E-Mail        : ' GET MAIL_CONJ
      READ
      @ 21,02 SAY 'Num.Cliente   : ' GET NUM_CLIE PICT '@R 9999999999'
      CLEAR GETS
      * QUADRO DE OBSERVA€åES DO CLIENTE - PARAMETROS
      IF BLOCO_CLI = .T.
         SAVE SCREEN TO eTELA_OBSERVA
         @ 09,01 CLEAR TO 21,79
         @ 09,01 TO       21,79
         DO PCENTRA WITH ' Observacoes Sobre o Cliente ',09
         DO PMENSAG WITH 'CTRL+W = Salvar / CTRL+Q = Sair'
         IF F_BLOQR()
            SET CURSOR ON
            eOLD = MEMO_OBS
            eNEW = MEMOEDIT(eOLD,10,02,20,78,.T.,"F_CEDITE")
            REPLACE MEMO_OBS WITH eNEW
            SET CURSOR OFF
            UNLOCK
         ENDIF
         DO PPARA
         RESTORE SCREEN FROM eTELA_OBSERVA
         UNLOCK
      ENDIF
   ENDIF
   SELE 1
   DO PMENSAG WITH 'Pressione Qualquer Tecla Para Finalizar ...'
   DO PPARA
   DO PLQUA WITH '01'
   DO PPERG WITH 'Continua (S/N) ? '
   IF sisRESP = 'S'
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
RESTORE SCREEN FROM TELA03
RETURN
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION KEYS_3
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         RET = 0
   ENDCASE
ENDIF
RETURN RET
******************************************************************
************************ funcao *********************************
FUNCTION F_CEDITE
PARAMETERS mode, line, column
PRIVATE keypress, ret2
RET2 = 0
KEYPRESS = LASTKEY()
IF MODE == 1 .OR. MODE == 2
   DO CASE
      CASE KEYPRESS == 273
         RET2 = 34
      CASE KEYPRESS == -4
         RET2 = 1
      CASE KEYPRESS == -5
         RET2 = 6
      CASE KEYPRESS == -8
         RET2 = 25
      CASE KEYPRESS == 301
         RET2 = 23
   ENDCASE
ENDIF
RETURN RET2
*************************  fim  *********************************
