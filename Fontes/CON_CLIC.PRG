*********************************************************************
* Sistema     : Gest�o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CON_CLIC.PRG
* Data        : 01/01/99
* Objetivo    : CONSULTA DE CLIENTES POR CODIGO
* PRG's       : ---
* DBF's       : ---
* Alteracao   :
*********************************************************************
SAVE SCREEN TO TELA03
DO PTIT WITH 'CONSULTA DE CLIENTES CADASTRADOS - POR C�DIGO'
SELE 1
USE CONTATO INDEX NUM_CLIE,CONTATO
GO TOP
DO WHILE .T.
   DO PLIMPA
   SET COLOR TO W/N,+W/R
   eNUM_CLIE = 0
   @ 05,02 SAY 'C�digo :' GET eNUM_CLIE PICT '9999999'
   READ
   IF eNUM_CLIE = 0
      RESTORE SCREEN FROM TELA03
      CLOSE DATABASES
      RETURN
   ENDIF
   SEEK eNUM_CLIE
   IF .NOT. FOUND()
      DO PLQUA WITH '01'
      DO PMSGR WITH 'Aten��o !',3
      DO PMENSAG WITH 'C�digo N�o Cadastrado'
      DO PPARA
      LOOP
   ENDIF
   IF F_BLOQR()
      @ 06,02 SAY 'Nome          : ' GET CLIENTE
      CLEAR GETS
      @ 07,02 SAY 'CPF/CGC       : ' GET CPF_CGC
      @ 08,02 SAY 'RG            : ' GET RG
      @ 08,44 SAY 'UF : '            GET UF_RG PICT '@!XX'
      @ 09,02 SAY 'Emiss�o       : ' GET EMISSAO_RG
      @ 09,41 SAY 'Org�o : '         GET ORGAO_RG
      @ 10,02 SAY 'Sexo (M/F/O)  : ' GET SEXO PICT 'X' VALID SEXO $('MFO')
      @ 10,36 SAY 'Nascimento : '    GET NASCIMENTO
      SET COLOR TO W/N+
      @ 12,02 SAY 'ENDERECO RESIDENCIAL'
      SET COLOR TO W/N,+W/R
      * TERCEIRA PAGINA
      @ 14,02 SAY 'Endere�o      : ' GET END_R
      @ 15,02 SAY 'Bairro        : ' GET BAIRRO_R
      @ 16,02 SAY 'Cidade        : ' GET CIDADE_R
      @ 17,02 SAY 'Estado        : ' GET ESTADO_R PICT '@!XX'
      @ 18,02 SAY 'CEP           : ' GET CEP_R   PICT '@R 99999-999'
      @ 19,02 SAY 'Telefone  # 1 : ' GET TEL_R_1 PICT '@R (XX) XXXXX-XXXXX'
      @ 20,02 SAY 'Telefone  # 2 : ' GET TEL_R_2 PICT '@R (XX) XXXXX-XXXXX'
      @ 21,02 SAY 'e-mail        : ' GET MAIL_PROP
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
      @ 11,02 SAY 'Endere�o      : ' GET END_C
      @ 12,02 SAY 'Bairro        : ' GET BAIRRO_C
      @ 13,02 SAY 'Cidade        : ' GET CIDADE_C
      @ 14,02 SAY 'Estado        : ' GET ESTADO_C PICT '@!XX'
      @ 15,02 SAY 'CEP           : ' GET CEP_C   PICT '@R 99999-999'
      @ 16,02 SAY 'Telefone  # 1 : ' GET TEL_C_1 PICT '@R (XX) XXXXX-XXXXX - (XXXXXXXXXXXXXXXXXXXX)'
      @ 17,02 SAY 'Telefone  # 2 : ' GET TEL_C_2 PICT '@R (XX) XXXXX-XXXXX - (XXXXXXXXXXXXXXXXXXXX)'
      @ 18,02 SAY 'E-Mail        : ' GET MAIL_C          
      @ 20,02 SAY 'Correspondencia na Residencia ou Escritorio (R/E) ? ' GET OPCAO PICT '@!X' VALID OPCAO $('RE')
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
      SET COLOR TO W/N+
      @ 14,02 SAY 'DADOS DO CONJUGE'
      SET COLOR TO W/N,+W/R
      @ 15,02 SAY 'Nome Conjuge  : ' GET CONJUGE
      @ 16,02 SAY 'Nascimento    : ' GET NASC_CONJ
      @ 17,02 SAY 'CPF Conjuge   : ' GET CPF_CONJ
      @ 18,02 SAY 'Telefone Conj : ' GET TEL_CONJ PICT '@R (XX) XXXXX-XXXXX - (XXXXXXXXXXXXXXXXXXXX)'
      @ 19,02 SAY 'e-mail        : ' GET MAIL_CONJ
      @ 21,02 SAY 'Num.Cliente   : ' GET NUM_CLIE PICT '@R 9999999999'
      CLEAR GETS
      UNLOCK
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
