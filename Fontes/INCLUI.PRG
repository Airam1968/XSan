*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : INCLUI.PRG
* Data        : 01/01/99
* Objetivo    : INCLUSAO DE CLIENTES
* PRG's       : ---
* DBF's       : ---
* Alteracao   :
***************************************************************************
SAVE SCREEN TO TELA03
DO PTIT WITH 'INCLUSAO DE CLIENTES'
SELE 5
USE CONTATO INDEX CONTATO,NUM_CLIE
GO TOP
DO WHILE .T.
   DO PLIMPA
   SET COLOR TO W/N,+W/R
   eCLIENTE = SPACE(50)
   @ 05,02 SAY 'Cliente     : ' GET eCLIENTE PICT '@! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
   READ
   SELE 5
   GO TOP
   eCLIENTE = UPPER(eCLIENTE)
   IF eCLIENTE = SPACE(50)
      EXIT
   ENDIF
   SEEK eCLIENTE
   IF FOUND()
      DO PLQUA WITH '01'
      DO PMSGR WITH 'Cliente j† existe',3
      DO PLQUA WITH '01'
      DO PPERG WITH 'Deseja Cadastrar Outro Cliente com o Mesmo NOME (S/N) ? '
      IF sisRESP = 'N'
	 LOOP
      ENDIF
   ENDIF
   eCPF_CGC    = SPACE(18)
   eRG         = SPACE(14)
   eUF_RG      = SPACE(2)
   eEMISSAO_RG = CTOD('')
   eORGAO_RG   = SPACE(10)
   eSEXO       = SPACE(1)
   eEND_R     = SPACE(50)
   eBAIRRO_R  = SPACE(20)
   eCIDADE_R  = SPACE(20)
   eESTADO_R  = SPACE(2)
   eCEP_R     = SPACE(8)
   eTEL_R_1   = SPACE(12)
   eTEL_R_2   = SPACE(12)
   eNASCIMENTO= CTOD('')
   eMAIL_PROP = SPACE(50)
   @ 07,02 SAY 'CPF ou CGC    : ' GET eCPF_CGC
   @ 08,02 SAY 'RG            : ' GET eRG
   @ 08,44 SAY 'UF : '         GET eUF_RG PICT '@!XX'
   @ 09,02 SAY 'Emiss∆o       : ' GET eEMISSAO_RG
   @ 09,41 SAY 'OrgÉo : '         GET eORGAO_RG
   @ 10,02 SAY 'Sexo (M/F/O)  : ' GET eSEXO PICT '@!X' VALID eSEXO $('MFO')
   @ 10,36 SAY 'Nascimento : '    GET eNASCIMENTO
   READ
   IF eSEXO = 'M' .OR. eSEXO = 'F'
      IF .NOT. TESTACPF(eCPF_CGC)
         DO PLQUA WITH '01'
         DO PMSGR WITH 'Atená∆o !!!',3
         DO PMENSAG WITH 'CPF nao confere !'
         DO PPARA
      ENDIF
   ELSE
      IF .NOT. TESTACGC(eCPF_CGC)
         DO PLQUA WITH '01'
         DO PMSGR WITH 'Atená∆o !!!',3
         DO PMENSAG WITH 'CGC nao confere !'
         DO PPARA
      ENDIF
   ENDIF
   SET COLOR TO W/N+
   @ 12,02 SAY 'ENDERECO RESIDENCIAL'
   SET COLOR TO W/N,+W/R
   @ 14,02 SAY 'Endereáo      : ' GET eEND_R
   @ 15,02 SAY 'Bairro        : ' GET eBAIRRO_R
   @ 16,02 SAY 'Cidade        : ' GET eCIDADE_R
   @ 17,02 SAY 'Estado        : ' GET eESTADO_R PICT '@!XX'
   @ 18,02 SAY 'CEP           : ' GET eCEP_R   PICT '@R 99999-999'
   @ 19,02 SAY 'Telefone  # 1 : ' GET eTEL_R_1 PICT '@R (XX) XXXXX-XXXXX'
   @ 20,02 SAY 'Telefone  # 2 : ' GET eTEL_R_2 PICT '@R (XX) XXXXX-XXXXX'
   @ 21,02 SAY 'E-Mail        : ' GET eMAIL_PROP
   READ
   DO PMENSAG WITH 'Pressione Qualquer Tecla Para Continuar ...'
   DO PPARA
   DO PLQUA WITH '01'
   ePROFISSAO = SPACE(30)
   eEMPRESA   = SPACE(50)
   eEND_C     = SPACE(50)
   eBAIRRO_C  = SPACE(20)
   eCIDADE_C  = SPACE(20)
   eESTADO_C  = SPACE(2)
   eCEP_C     = SPACE(15)
   eTEL_C_1   = SPACE(40)
   eTEL_C_2   = SPACE(40)
   eOPCAO     = SPACE(1)
   eCONJUGE   = SPACE(50)
   eCPF_CONJ  = SPACE(14)
   eTEL_CONJ  = SPACE(40)
   eMAIL_CONJ = SPACE(50)
   eNASC_CONJ = CTOD('') 
   eMAIL_C    = SPACE(50)   
   @ 06,01 CLEAR TO 21,79
   SET COLOR TO W/N+
   @ 07,02 SAY 'ENDERECO COMERCIAL'
   SET COLOR TO W/N,+W/R
   * QUARTA PAGINA
   @ 09,02 SAY 'Empresa       : ' GET eEMPRESA
   @ 10,02 SAY 'Profissao     : ' GET ePROFISSAO
   @ 11,02 SAY 'Endereáo      : ' GET eEND_C
   @ 12,02 SAY 'Bairro        : ' GET eBAIRRO_C
   @ 13,02 SAY 'Cidade        : ' GET eCIDADE_C
   @ 14,02 SAY 'Estado        : ' GET eESTADO_C PICT '@!XX'
   @ 15,02 SAY 'CEP           : ' GET eCEP_C   PICT '@R 99999-999'
   @ 16,02 SAY 'Telefone  # 1 : ' GET eTEL_C_1 PICT '@R (XX) XXXXX-XXXXX - (XXXXXXXXXXXXXXXXXXXX)'
   @ 17,02 SAY 'Telefone  # 2 : ' GET eTEL_C_2 PICT '@R (XX) XXXXX-XXXXX - (XXXXXXXXXXXXXXXXXXXX)'
   @ 18,02 SAY 'E-Mail        : ' GET eMAIL_C   
   @ 20,02 SAY 'Correspondencia na Residencia ou Escritorio (R/E) ? ' GET eOPCAO PICT '@!X' VALID eOPCAO $('RE')
   READ
   DO PMENSAG WITH 'Pressione Qualquer Tecla Para Finalizar ...'
   DO PPARA
   eOBSERVA_1 = SPACE(80)
   eOBSERVA_2 = SPACE(80)
   eOBSERVA_3 = SPACE(80)
   eOBSERVA_4 = SPACE(80)
   eOBSERVA_5 = SPACE(80)
   @ 07,01 CLEAR TO 21,79
   SET COLOR TO W/N+
   @ 07,02 SAY 'OBSERVACOES GERAIS'
   SET COLOR TO W/N,+W/R
   * QUINTA PAGINA
   @ 08,00 GET eOBSERVA_1
   @ 09,00 GET eOBSERVA_2
   @ 10,00 GET eOBSERVA_3
   @ 11,00 GET eOBSERVA_4
   @ 12,00 GET eOBSERVA_5
   READ
   SET COLOR TO W/N+
   @ 14,02 SAY 'DADOS DO CONJUGE'
   SET COLOR TO W/N,+W/R
   @ 15,02 SAY 'Nome Conjuge  : ' GET eCONJUGE
   @ 16,02 SAY 'Data Nascim.  : ' GET eNASC_CONJ
   @ 17,02 SAY 'CPF Conjuge   : ' GET eCPF_CONJ
   @ 18,02 SAY 'Telefone Conj : ' GET eTEL_CONJ PICT '@R (XX) XXXXX-XXXXX - (XXXXXXXXXXXXXXXXXXXX)'
   @ 19,02 SAY 'e-mail        : ' GET eMAIL_CONJ
   READ
   SELE 9
   USE STATUS
   GOTO TOP
   eNUM_CLIE = NUM_CLIE
   @ 21,02 SAY 'Num.Cliente   : ' GET eNUM_CLIE PICT '@R 9999999999'
   CLEAR GETS
   SELE 5
   DO PMENSAG WITH 'Pressione Qualquer Tecla Para Finalizar ...'
   DO PPARA
   DO PLQUA WITH '01'
   DO PPERG WITH 'Confirma (S/N) ? '
   IF sisRESP = 'S'
      DO PMENSAG WITH 'GRAVANDO, AGUARDE ...'
      SELE 5
      APPEND BLANK
      IF F_BLOQR()
	 REPLACE CLIENTE    WITH eCLIENTE,;
		 CPF_CGC    WITH eCPF_CGC,;
		 RG         WITH eRG
	 REPLACE UF_RG      WITH eUF_RG,;
		 EMISSAO_RG WITH eEMISSAO_RG,;
		 ORGAO_RG   WITH eORGAO_RG
	 REPLACE SEXO       WITH eSEXO,;
		 END_R      WITH eEND_R,;
		 BAIRRO_R   WITH eBAIRRO_R
	 REPLACE CIDADE_R   WITH eCIDADE_R,;
		 CEP_R      WITH eCEP_R,;
		 TEL_R_1    WITH eTEL_R_1
         REPLACE ESTADO_R   WITH eESTADO_R,;
                 ESTADO_C   WITH eESTADO_C
	 REPLACE TEL_R_2    WITH eTEL_R_2,;
		 END_C      WITH eEND_C,;
		 BAIRRO_C   WITH eBAIRRO_C
	 REPLACE CIDADE_C   WITH eCIDADE_C,;
		 CEP_C      WITH eCEP_C,;
		 TEL_C_1    WITH eTEL_C_1
	 REPLACE TEL_C_2    WITH eTEL_C_2,;
		 OBSERVA_1  WITH eOBSERVA_1
	 REPLACE OBSERVA_2  WITH eOBSERVA_2,;
		 OBSERVA_3  WITH eOBSERVA_3,;
		 OBSERVA_4  WITH eOBSERVA_4
	 REPLACE OBSERVA_5  WITH eOBSERVA_5,;
		 OPCAO      WITH eOPCAO,;
		 EMPRESA    WITH eEMPRESA
         REPLACE NASCIMENTO WITH eNASCIMENTO,;
                 PROFISSAO  WITH ePROFISSAO,;
                 CONJUGE    WITH eCONJUGE
         REPLACE NASC_CONJ  WITH eNASC_CONJ,;
                 CPF_CONJ   WITH eCPF_CONJ,;
                 TEL_CONJ   WITH eTEL_CONJ
         REPLACE MAIL_CONJ  WITH eMAIL_CONJ,;
                 MAIL_PROP  WITH eMAIL_PROP,;
                 MAIL_C     WITH eMAIL_C                 
	 COMMIT
	 UNLOCK
      ENDIF
      SELE 9
      USE STATUS
      GOTO TOP
      IF F_BLOQR()
	 eNUM_CLIE = NUM_CLIE
	 REPLACE NUM_CLIE WITH eNUM_CLIE + 1
	 UNLOCK
      ENDIF
      SELE 5
      IF F_BLOQR()
	 REPLACE NUM_CLIE WITH eNUM_CLIE
	 UNLOCK
      ENDIF
      F_LACESSO(sisUSUARIO,'CADASTRO CLIENTES','CADASTRO CLIENTE No. '+ALLTRIM(STR(eNUM_CLIE,10))) 
      SELE 5
   ENDIF
ENDDO
RESTORE SCREEN FROM TELA03
RETURN
*************************  fim  *********************************


