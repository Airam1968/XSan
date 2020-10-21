*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : CAD_PAGT.PRG
* Data        : 01/01/1999
* Objetivo    : CADASTRO DE PAGAMENTO DE PARCELAS BALCAO
* PRG's       : ---
* DBF's       : ---
* Alteracao   :
*********************************************************************
SAVE SCREEN TO TELA03
DO PTIT WITH 'CADASTRO PAGAMENTO DE PARCELAS - TS'
SELE 1
USE UNI_IMOB INDEX UNI_IMOB ALIAS S1
GOTO TOP
SELE 2
USE CLAS_PRO INDEX CLAS_PRO
GOTO TOP
SELE 3
USE CAD_QUAD INDEX CAD_QUAD
GOTO TOP
SELE 4
USE CAD_APAR INDEX CAD_APAR
GOTO TOP
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SELE 6
USE CAD_VEND INDEX CAD_VEND,DATA_VEN
GOTO TOP
SELE 7
USE TAXA_MES INDEX TAXA_MES
GOTO TOP
SELE 8
USE STATUS
eANATOCISMO = ANATOCISMO
eCPF_CGC    = CGC_CPF
eTX_MULTA   = MULTA/100
eTX_DESCON  = DESCONTO/100
eCIDADE     = CIDADE
eCORR_ATRA  = CORR_ATRA
ePRO_RATA   = PRO_RATA
eCORRECAO   = 0
eDIA_VECC   = 0
eMES_VECC   = 0
eANO_VECC   = 0
eDIA_VENC   = 0
eMES_VENC   = 0
eANO_VENC   = 0
eDIA_PAGO   = 0
eMES_PAGO   = 0
eANO_PAGO   = 0
eNUM_BANCO  = SPACE(3)
eNUM_AGENC  = SPACE(10)
eNUM_CONTA  = SPACE(15)
eNUM_CHEQUE = SPACE(15)
USE
SELE 8
* Cadastro de Parcelas
SELE 2
* Selecao do produto
DO WHILE .T.
   DO PLIMPA
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   @ 05,02 SAY 'Produto : '
   GO TOP
   DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]='@R99999'
   PIC[2]='@R99999'
   PIC[3]='@S40'
   CAB[1]='    '
   CAB[2]='    '
   CAB[3]='    '
   DBEDIT(5,15,5,70,CAMPO,'FUNC_PROV',PIC,CAB,' ',' - ',' ')
   eCLAS_PRO  = CODIGO
   eTIPO_PRO  = TIPO
   eCONTA     = COD_CONTA
   eCONTA_ORI = COD_CONTA
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
eBLOCO  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      DO WHILE .T. 
         * Consulta de Produtos
	 SELE 3
	 SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
	 GOTO TOP
	 eQUADRA   = 0
         @ 06,02 SAY sisGRUPO1+'  : '
         GO TOP 
         DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@R99999"
         PIC[4]="@S99999"
         PIC[5]="@S1"  
         PIC[6]="@S20"         
         CAB[1]="    "
         CAB[2]="    "
         CAB[3]="    "
         CAB[4]="    "
         CAB[5]=" "   
         CAB[6]="    "        
         DBEDIT(6,14,6,75,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
	 eQUADRA     = NUM_QUADRA
	 eNUM_INICIO = NUM_INICIO
	 eNUM_FINAL  = NUM_FINAL
         @ 07,02 SAY sisUNID1+'   : '
	 SELE 1
	 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
	 GOTO TOP
	 IF EOF()
	    DO PLQUA WITH '01'
	    DO PMSGR WITH 'Atená∆o !',3
            DO PMENSAG WITH 'N∆o H† '+sisUNID1+' Vendidos !'
	    DO PPARA
	    CLOSE DATABASES
	    RESTORE SCREEN FROM TELA03
	    RETURN
	 ENDIF 
	 DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
	 AFIELDS(CAMPO,.T.,LENS)
	 CAMPO[5]='PROPRIET_1'
	 CAMPO[6]='PROPRIET_2'
	 LENS[5] =5
	 LENS[6] =5
	 PIC[1]='@R99999'
	 PIC[2]='@R99999'
	 PIC[3]='@R99999'
	 PIC[4]='@R99999'
	 PIC[5]='@R99999'
	 PIC[6]='@R99999'
	 CAB[1]='     '
	 CAB[2]='     '
	 CAB[3]='     '
	 CAB[4]='     '
	 CAB[5]='     '
	 CAB[6]='     '
	 DBEDIT(7,15,7,69,CAMPO,'FUNC_PROV',PIC,CAB,' ',' - ',' ')
	 eNUM_IMOVEL = NUM_IMOVEL
	 eSTATUS     = STATUS
	 ePROPR_1    = PROPRIET_1
	 ePROPR_2    = PROPRIET_2
	 IF eSTATUS = 0
	    IF ePROPR_1 = 1
	       DO PLQUA WITH '01'
	       DO PMSGR WITH 'Atená∆o !',3
               DO PMENSAG WITH  sisUNID1+' nao Vendido !'
	       DO PPARA
	    ELSE
	       ePARTE = 0
	       F_PAGB_TS(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_1)
	    ENDIF  
	 ELSE
	    eOPCAO = 0
	    DO PLQUA WITH '01'
            @ 23,00 SAY 'Informe a Venda para o (1)o. ou (2)o. Meio '+sisUNID1+' ? ' GET eOPCAO PICT '99999' RANGE 1,2
	    READ
	    IF eOPCAO = 1
	       IF ePROPR_1 = 1
		  DO PLQUA WITH '01'
		  DO PMSGR WITH 'Atená∆o !',3
                  DO PMENSAG WITH sisUNID1+' nao Vendido !'
		  DO PPARA
	       ELSE
		  ePARTE = 1
		  F_PAGB_TS(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_1)
	       ENDIF  
	    ELSE
	       IF ePROPR_2 = 1
		  DO PLQUA WITH '01'
		  DO PMSGR WITH 'Atená∆o !',3
                  DO PMENSAG WITH sisUNID2+' nao Vendido !'
		  DO PPARA
	       ELSE
		  ePARTE = 2
		  F_PAGB_TS(eCLAS_PRO,eQUADRA,eNUM_IMOVEL,ePARTE,ePROPR_2)
	       ENDIF  
	    ENDIF
	 ENDIF
	 DO PLQUA WITH '01'
         DO PPERG WITH ' Deseja  Continuar  no  Mesmo  Produto  (S/N) ?  '
	 IF sisRESP = 'S'
	    @ 06,00 CLEAR TO 21,79
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   CASE eTIPO_PRO = 2
      DO WHILE .T.
	 * Consulta de Apartamentos
	 SELE 4
	 SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
	 GOTO TOP
	 eBLOCO    = 0
         @ 06,02 SAY sisGRUPO1+'   : '
	 GO TOP
	 DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
	 AFIELDS(CAMPO,.T.,LENS)
	 PIC[1]='@R99999'
	 PIC[2]='@R99999'
	 PIC[3]='@S30'
	 PIC[4]='@R99999'
	 CAB[1]='    '
	 CAB[2]='    '
	 CAB[3]='    '
	 CAB[4]='    '
	 DBEDIT(6,08,6,76,CAMPO,'FUNC_PROV',PIC,CAB,' ',' - ',' ')
	 eBLOCO      = NUM_BLOCO
	 eQUANTIDADE = QUANTIDADE
         @ 07,02 SAY sisUNID1+'   : '
	 SELE 1
	 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eBLOCO .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1))
	 GOTO TOP
	 IF EOF()
	    DO PLQUA WITH '01'
	    DO PMSGR WITH 'Atená∆o !',3
            DO PMENSAG WITH 'N∆o H† '+sisPROD1+' Vendidos !'
	    DO PPARA
	    CLOSE DATABASES
	    RESTORE SCREEN FROM TELA03
	    RETURN
	 ENDIF 
	 DECLARE CAMPO[5],PIC[5],CAB[5],LENS[5]
	 AFIELDS(CAMPO,.T.,LENS)
	 CAMPO[5]='PROPRIET_1'
	 LENS[5] =5
	 PIC[1]='@R99999'
	 PIC[2]='@R99999'
	 PIC[3]='@R99999'
	 PIC[4]='@R99999'
	 PIC[5]='@R99999'
         CAB[1]=' Produto '
         CAB[2]='Grupo'
         CAB[3]='Unid.'
	 CAB[4]='Status'
	 CAB[5]='Proprietario'
	 DBEDIT(7,14,7,64,CAMPO,'FUNC_PROV',PIC,CAB,' ',' - ',' ')
	 eNUM_IMOVEL = NUM_IMOVEL
	 eSTATUS     = STATUS
	 ePROPR_1    = PROPRIET_1
	 ePARTE      = 0
	 IF ePROPR_1 = 1
	    DO PLQUA WITH '01'
	    DO PMSGR WITH 'Atená∆o !',3
            DO PMENSAG WITH sisUNID1+' nao Vendido !'
	    DO PPARA
	 ELSE
	    F_PAGB_TS(eCLAS_PRO,eBLOCO,eNUM_IMOVEL,0,ePROPR_1) 
	 ENDIF
	 DO PLQUA WITH '01'
         DO PPERG WITH ' Deseja  Continuar  no  Mesmo  Produto  (S/N) ?  '
	 IF sisRESP = 'S'
	    @ 06,00 CLEAR TO 21,79
	    LOOP
	 ELSE
	    EXIT
	 ENDIF
      ENDDO
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN 
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_PAGB_TS
PARAMETERS pCLASSE,pQUADRA,pUNIDADE,pPARTE,pNUM_CLIE
eMSG = 'RELACAO DAS PARCELAS DA UNIDADE No. '+ALLTRIM(pUNIDADE)+' - '+IIF(pPARTE=0,'Proprietario Unico',IIF(pPARTE=1,'1o. PROPRIETARIO','2o. PROPRIETARIO'))
DO PCENTRA WITH eMSG,9
SELE 5
USE CONTATO INDEX NUM_CLIE,CONTATO
GOTO TOP
SEEK pNUM_CLIE
@ 08,02 SAY 'Cliente : '+ALLTRIM(STR(pNUM_CLIE,5))+' - '+CLIENTE
eCLIENTE = CLIENTE
eSEXO    = SEXO
SELE 8
USE STATUS
eCPF_CGC   = CGC_CPF
eTX_MULTA  = MULTA/100
eTX_DESCON = DESCONTO/100
eCIDADE    = CIDADE
eCORR_ATRA = CORR_ATRA
ePRO_RATA  = PRO_RATA
SELE 8
eIND_CLAS = STR(pCLASSE,5)
eIND_QUAD = pQUADRA
eZERO = '000'
eARQ  = 'CP'+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND1 = 'IG'+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND2 = 'IV'+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
eIND3 = 'IP'+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
USE &eARQ INDEX &eIND2,&eIND1,&eIND3
GOTO TOP
SET FILTER TO CLASSE = STR(pCLASSE,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pPARTE
GOTO TOP
SELE 6
GOTO TOP
SEEK STR(pCLASSE,5)+pQUADRA+pUNIDADE+STR(pPARTE,1)
eINDICE     = INDICE
eOBSERVACAO = OBSERVACAO
IF eOBSERVACAO <> SPACE(60)
   SAVE SCREEN TO eTELA_OBSERVA
   DO PLQUA WITH '01'
   DO PMSGR WITH 'Atená∆o !!!',3
   DO PLQUA WITH '01'
   SET COLOR TO GR+/N+
   DO PCENTRA WITH 'OBSERVAÄ«O : '+ALLTRIM(eOBSERVACAO),23
   SET COLOR TO W/N,+W/R
   DO PPARA
   DO PLQUA WITH '01'
   DO PPERG WITH 'Deseja visualizar o bloco de notas desta venda (S/N) ? '
   IF sisRESP = 'S'
      * QUADRO DE PROCESSO
      SELE 1
      DECLARE CAMPO[8],PIC[8],CAB[8],LENS[8]
      AFIELDS(CAMPO,.T.,LENS)
      CAMPO[5]='PROPRIET_1'
      CAMPO[6]='PROCESSO_1'
      CAMPO[7]='PROPRIET_2'
      CAMPO[8]='PROCESSO_2'
      LENS[5]=5
      LENS[6]=1
      LENS[7]=5
      LENS[8]=1
      PIC[1]='@R99999'
      PIC[2]='@R99999'
      PIC[3]='@R99999'
      PIC[4]='@R99999'
      PIC[5]='@R99999'
      PIC[6]='@R99999'
      CAB[1]=' Produto '
      CAB[2]='Grupo'
      CAB[3]='Unid'
      CAB[4]='Status'
      CAB[5]='1o. Proprietario'
      CAB[6]='Processo 1o.'
      CAB[7]='2o. Proprietario'
      CAB[8]='Processo 2o.'
      @ 12,00 TO 18,79 DOUBLE
      DBEDIT(13,01,18,78,CAMPO,'FUNC_PROPR3',PIC,CAB,CHR(196),CHR(179),CHR(205))
      SELE 6
      RESTORE SCREEN FROM eTELA_OBSERVA
   ENDIF
ENDIF
SELE 8
GOTO TOP
DO PLQUA WITH '01'
DO PPERG WITH 'Deseja Localizar a 1a. Parcela em Aberto (S/N) ? '
IF sisRESP = 'S'
   LOCATE FOR DATA_PGTO = CTOD('') .AND. ATIVA
ENDIF
DECLARE CAMPO[16],PIC[16],CAB[16],LENS[16]
AFIELDS(CAMPO,.T.,LENS)
CAMPO[1]='NUM_PARC'
CAMPO[2]='TIPO_PARC'
CAMPO[3]='DATA_VENC'
CAMPO[4]='VLR_PARC'
CAMPO[5]='DATA_PGTO'
CAMPO[6]='VLR_PGTO'
CAMPO[7]='COD_BANCO'
CAMPO[8]='ORIGEM'
CAMPO[9]='PROTESTO'
CAMPO[10]='CONTABIL'
CAMPO[11]='OBS'
CAMPO[12]='MULTA'
CAMPO[13]='COMISSAO'
CAMPO[14]='PERID_COMI'
CAMPO[15]='PERCENTUAL' 
CAMPO[16]='CONTA'
LENS[1]=5
LENS[2]=3
LENS[3]=8
LENS[4]=19
LENS[5]=8
LENS[6]=19
LENS[7]=20
LENS[8]=1
LENS[9]=1
LENS[10]=1
LENS[11]=10
LENS[12]=10
LENS[13]=10
LENS[14]=4
LENS[15]=10
LENS[16]=3
PIC[1]='@R99999'
PIC[2]='XXX'
PIC[3]='99/99/99'
PIC[4]='@R 99,999,999,999.99'
PIC[5]='99/99/99'
PIC[6]='@R 99,999,999,999.99'
PIC[7]='@R 99999999999999'
PIC[8]='@!X'
PIC[9]='L'
PIC[10]='L'
PIC[11]='XXXXXXXXXX'
PIC[12]='@R 999.99'
PIC[13]='@R 999.999'
PIC[14]='XXXX'
PIC[15]='@R 999.99'
PIC[16]='@R 999'
CAB[1] ='Parcela'
CAB[2] ='Tipo(N/I)'
CAB[3] ='Venc.'
CAB[4] ='Parcela'
CAB[5] ='Pago'
CAB[6] ='Vlr. Pagamento'
CAB[7] ='Cod. Banco'
CAB[8] ='Origem'
CAB[9] ='Protesto'
CAB[10]='Contabil'
CAB[11]='Obs.'
CAB[12]='Multa'
CAB[13]='Tx. perm.'
CAB[14]='Perid.'
CAB[15]='Tx. Comis.'
CAB[16]='Cod.Conta'
@ 10,00 TO 21,79 DOUBLE
*  Alteraá∆o para Terra Sim∆o
IF sisCGC_CPF = '00915210000100'
   DBEDIT(11,01,21,78,CAMPO,'FUNC_PAG2_TS',PIC,CAB,CHR(196),CHR(179),CHR(205))
ELSE
   DBEDIT(11,01,21,78,CAMPO,'FUNC_PAGB_TS',PIC,CAB,CHR(196),CHR(179),CHR(205))
ENDIF
RETURN 0
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION FUNC_PAGB_TS
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         IF DB_COLUMN <= 8
            SAVE SCREEN TO TELA04
               IF F_BLOQR()
               eACESSOMASTER = .F.
               IF DATA_PGTO <> CTOD('') .AND. (sisCGC_CPF = '02852392000142' .OR. sisCGC_CPF = '06983112000196')
                  *------------------------- inicio -------------------------
                  * Alteracao para Cibele da S¢lida/Delly
                  SET CURSOR ON
                  DO PLQUA WITH '01'
                  DO PMSGR WITH 'Atená∆o !',3
                  DO PMENSAG WITH 'Senha do Master Requerida !'
                  DO PPARA
                  SAVE SCREEN TO TELASENHA
                  DO PLIMPA
                  SELE 9
                  USE CAD_USU INDEX CAD_USU
                  GOTO TOP
                  eACESSO = .F.
                  eVEZES  = 1
                  DO WHILE eVEZES <= 3
                     eUSUARIO = SPACE(10)
                     eSENHA   = SPACE(10)
                     eUSUARIO = 'MASTER    '
                     @ 11,24 TO 16,56
                     SET COLOR TO W/R+,+W/W
                     SET INTE ON
                     DO PCENTRA WITH 'Controle de Acesso',11
                     SET COLOR TO W/N,+W/N
                     DO WHILE .T.
                        @ 13,30 SAY 'Usu†rio : ' GET eUSUARIO PICT '@!XXXXXXXXXX'
                        CLEAR GETS
                        IF eUSUARIO = SPACE(10)
                           LOOP
                        ELSE
                           EXIT
                        ENDIF
                     ENDDO
                     SET COLOR TO
                     SET COLOR TO W/N,X/X
                     DO WHILE .T.
                        @ 14,30 SAY 'Senha   : '
                        @ 14,41 GET eSENHA   PICT '@!XXXXXXXXXX'
                        READ
                        IF eSENHA = SPACE(10)
                           LOOP
                        ELSE
                           EXIT
                        ENDIF
                     ENDDO
                     SET COLOR TO W/N,+W/R
                     SET INTE ON
                     SELE 9
                     SEEK eUSUARIO
                     IF FOUND()
                        IF eSENHA = SENHA
                           RESTORE SCREEN FROM TELASENHA
                           eACESSOMASTER = .T.
                           EXIT
                        ELSE
                           DO PLIMPA WITH '01'
                           DO PMSGR WITH 'Senha Inv†lida !',3
                           eVEZES = eVEZES + 1
                           LOOP
                        ENDIF
                     ELSE
                        DO PLIMPA WITH '01'
                        DO PMSGR WITH 'Usu†rio Inv†lido !',3
                        eVEZES = eVEZES + 1
                        LOOP
                     ENDIF
                  ENDDO
               ELSE
                  eACESSOMASTER = .T.
               ENDIF
               IF eACESSOMASTER = .F.
                  SET CURSOR OFF
                  RESTORE SCREEN FROM TELA04
                  SELE 8
                  RETURN
               ENDIF
               *-------------------------  fim  ---------------------------
               SELE 8
               SET CURSOR ON
               DO PLQUA WITH '01'
               DO PPERG WITH 'Pagamento de Parcela (S/N) ? '
               IF sisRESP = 'S'
                  IF .NOT. ATIVA
                     DO PLQUA WITH '01'
                     DO PMSGR WITH 'Atená∆o !',3
                     DO PMENSAG WITH 'N∆o Ç Possivel Pagar uma Parcela Inativa !'
                     DO PPARA
                  ELSE
                     eAUX_ACUM   = 0
                     ePERID_COMI = PERID_COMI
                     eTX_MULTA   = MULTA
                     eTX_COMPER  = COMISSAO
                     eVLR_PARC   = VLR_PARC
                     eDATA_VENC  = DATA_VENC
                     eVLR_PAGO   = 0
                     eDATA_PAGO  = DATE()
                     eMULTA      = 0
                     eJUROS      = 0
                     eDESCONTO   = 0
                     eAUX        = 0
                     @ 10,02 CLEAR TO 22,78
                     @ 10,02 TO 22,78 DOUBLE
                     DO PCENTRA WITH "VALORES DA PARCELA",11
                     @ 12,12 SAY 'Valor da Parcela   : ' GET eVLR_PARC PICT '@R 999,999,999.99'
                     @ 13,12 SAY 'Data de Vencimento : ' GET eDATA_VENC
                     CLEAR GETS
                     @ 14,12 SAY 'Data de Pagamento  : ' GET eDATA_PAGO
                     READ
                     DO CASE
                        CASE eDATA_VENC = eDATA_PAGO
                           DO PLQUA WITH '01'
                           DO PMENSAG WITH 'Pagamento dentro do Prazo, Aguarde ... 1'
                           eDESCONTO = 0
                           eMULTA    = 0
                           eJUROS    = 0
                           eCORRECAO = 0
                        CASE eDATA_VENC > eDATA_PAGO
                           * Calculo de Desconto de Parcelas
                           DO PLQUA WITH '01'
                           DO PMENSAG WITH 'Pagamento Antecipado, Aguarde ... 2'
                           eDIA_VENC = 0
                           eMES_VENC = 0
                           eANO_VENC = 0
                           eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
                           eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
                           eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
                           eDIA_VENC = VAL(eDIA_VENC)
                           eMES_VENC = VAL(eMES_VENC) - 1
                           eANO_VENC = VAL(eANO_VENC) 
                           IF eMES_VENC = 0
                              eMES_VENC = 12
                              eANO_VENC = eANO_VENC - 1
                           ENDIF
                           eDIA_VECC = eDIA_VENC
                           eMES_VECC = eMES_VENC
                           eANO_VECC = eANO_VENC
                           eTAM_DIA  = LENNUM(INT(eDIA_VENC))
                           eTAM_MES  = LENNUM(INT(eMES_VENC))
                           eDIA_VENC = IIF(eTAM_DIA = 1,'0'+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
                           eMES_VENC = IIF(eTAM_MES = 1,'0'+STR(eMES_VENC,1),STR(eMES_VENC,2))
                           eANO_VENC = STR(INT(eANO_VENC),4)
                           eMES_ANT  = CTOD(eDIA_VENC+'/'+eMES_VENC+'/'+eANO_VENC)
                           DO CASE
                              CASE eDATA_VENC > eDATA_PAGO .AND. eDATA_PAGO < eMES_ANT
                                 DO PLQUA WITH '01'
                                 DO PMENSAG WITH 'Calculo de Desconto, Aguarde ...'
                                 eMULTA    = 0
                                 eJUROS    = 0
                                 eCORRECAO = 0
                                 eAUX      = 1
                                 eDIA_VENC = 0
                                 eMES_VENC = 0
                                 eANO_VENC = 0
                                 eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
                                 eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
                                 eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
                                 eDIA_PAGO = 0
                                 eMES_PAGO = 0
                                 eANO_PAGO = 0
                                 eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
                                 eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
                                 eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
                                 eAUX_VENC = CTOD('')
                                 eDIA_VENC = VAL(eDIA_VENC)
                                 eMES_VENC = VAL(eMES_VENC)
                                 eANO_VENC = VAL(eANO_VENC)
                                 DO WHILE .T.
                                    eMES_VENC = eMES_VENC - 1
                                    IF eMES_VENC = 0
                                       eMES_VENC = 12
                                       eANO_VENC = eANO_VENC - 1
                                    ENDIF
                                    eAUX      = eAUX + 1
                                    eTAM_DIA  = LENNUM(INT(eDIA_VENC))
                                    eTAM_MES  = LENNUM(INT(eMES_VENC))
                                    eDIA_VENC = IIF(eDIA_VENC > 28 .AND. eMES_VENC = 2,28,eDIA_VENC)
                                    eDIA_VENC = IIF(eTAM_DIA = 1,'0'+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
                                    eMES_VENC = IIF(eTAM_MES = 1,'0'+STR(eMES_VENC,1),STR(eMES_VENC,2))
                                    eANO_VENC = STR(INT(eANO_VENC),4)
                                    eAUX_VENC = CTOD(eDIA_VENC+'/'+eMES_VENC+'/'+eANO_VENC)
                                    eDIA_VENC = VAL(IIF(eTAM_DIA = 1,'0'+ALLTRIM(eDIA_VENC),ALLTRIM(eDIA_VENC)))
                                    eMES_VENC = VAL(IIF(eTAM_MES = 1,'0'+ALLTRIM(eMES_VENC),ALLTRIM(eMES_VENC)))
                                    eANO_VENC = VAL(eANO_VENC)
                                    IF MONTH(eAUX_VENC) = MONTH(eDATA_PAGO) .AND. YEAR(eAUX_VENC) = YEAR(eDATA_PAGO)
                                       EXIT
                                    ENDIF
                                 ENDDO
                                 IF eAUX_VENC < eDATA_PAGO
                                    eAUX = eAUX - 1
                                 ENDIF
                                 * Calculo do desconto
                                 eDESCONTO = (eVLR_PARC * (eTX_DESCON * eAUX))
                                 eMULTA    = 0
                                 eJUROS    = 0
                                 eCORRECAO = 0
                              OTHERWISE
                                 eDESCONTO = 0
                                 eMULTA    = 0
                                 eJUROS    = 0
                                 eCORRECAO = 0
                           ENDCASE
                        OTHERWISE
                           * Calculo de Multa e Juros
                           DO CASE
                              CASE eDATA_PAGO > eDATA_VENC
                                 DO PLQUA WITH '01'
                                 DO PMENSAG WITH 'Calculo de Multa e Juros, Aguarde ...3'
                                 eDESCONTO = 0
                                 eMULTA    = 0
                                 eJUROS    = 0
                                 eCORRECAO = 0
                                 eAUX      = 1
                                 
                                 eDIA_VENC = ''
                                 eMES_VENC = ''
                                 eANO_VENC = ''
                                 eDIA_VENC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),1,2))
                                 eMES_VENC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),4,2))
                                 eANO_VENC = ALLTRIM(SUBSTR(DTOC(eDATA_VENC),7,4))
                                 
                                 eDIA_PAGO = ''
                                 eMES_PAGO = ''
                                 eANO_PAGO = ''
                                 eDIA_PAGO = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),1,2))
                                 eMES_PAGO = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),4,2))
                                 eANO_PAGO = ALLTRIM(SUBSTR(DTOC(eDATA_PAGO),7,4))
                                 
                                 eDIF_DIAS_TOTAL = 0
                                 eDIF_DIAS_TOTAL = eDATA_PAGO - eDATA_VENC
                                
                                 eCORRECAO = 0
                                 eJUROS = 0
                                 eMULTA = 0
                                 eDESCONTO = 0
                                 *Verifica se o pagamento Ç fora ou dentro do Màs
                                 IF (MONTH(eDATA_VENC) = MONTH(eDATA_PAGO)) .AND. (YEAR(eDATA_VENC) = YEAR(eDATA_PAGO))
                                    * Pagamento dentro do Mes
                                    * Data de Vencimento
                                    eTAM_DIA  = 0
                                    eTAM_MES  = 0
                                    eTAM_DIA  = LEN(eDIA_VENC)
                                    eTAM_MES  = LEN(eMES_VENC)

                                    eDIA_VENC = IIF(eTAM_DIA = 1,'0'+SUBSTR(eDIA_VENC,1,1),SUBSTR(eDIA_VENC,1,2))
                                    
                                    eMES_VENC = IIF(eTAM_MES = 1,'0'+SUBSTR(eMES_VENC,1,1),SUBSTR(eMES_VENC,1,2))
                                    
                                    eANO_VENC = SUBSTR(eANO_VENC,1,4)

                                    * Data de Pagamento
                                    eTAM_DIA  = LEN(eDIA_PAGO)
                                    eTAM_MES  = LEN(eMES_PAGO)

                                    eDIA_PAGO = IIF(eTAM_DIA = 1,'0'+SUBSTR(eDIA_PAGO,1,1),SUBSTR(eDIA_PAGO,1,2))
                                    
                                    eMES_PAGO = IIF(eTAM_MES = 1,'0'+SUBSTR(eMES_PAGO,1,1),SUBSTR(eMES_PAGO,1,2))
                                    
                                    eANO_PAGO = SUBSTR(eANO_PAGO,1,4)

                                    * Data Contagem 
                                    eTAM_DIA  = 0
                                    eTAM_MES  = 0
                                    eTAM_DIA  = LEN(eDIA_VENC)
                                    eTAM_MES  = LEN(eMES_VENC)

                                    eDIA_CONT = '01'
                                    IF eMES_VENC+1 = 13
                                       eMES_CONT = '01'
                                       eANO_CONT = SUBSTR(STR(VAL(eANO_VENC+1)),1,4)                                       
                                    ELSE   
                                       eMES_CONT = IIF(eTAM_MES = 1,'0'+SUBSTR(eMES_VENC+1,1,1),SUBSTR(eMES_VENC+1,1,2))
                                       eANO_CONT = SUBSTR(eANO_VENC,1,4)                                       
                                    ENDIF
                                    eDATA_CONT = CTOD(eDIA_CONT+'/'+eMES_CONT+'/'+eANO_CONT)
                                    eDATA_CONT = eDATA_CONT - 1
                                    eDIF_DIAS_MES = VAL(SUBSTR(DTOC(eDATA_CONT),1,2))                
                                    ?eDATA_CONT
                                    ?eDIF_DIAS_MES
                                    PPARA()
                                    
                                    * Calculo do Indice Acumulado
				    SELE 7
				    GOTO TOP
				    SET FILTER TO ((ANO+MES >= eANO_VENC+eMES_VENC) .AND. (ANO+MES <= eANO_PAGO+eMES_PAGO))
				    GOTO TOP
				    eIND_MES   = 1
				    
				    * Calculo da Correá∆o
				    *=1000+((2,5/31*10)*1000/100) Parcela+(°ndice/dias do màs*dias de atraso)*parcela%)
				    DO WHILE .NOT. EOF()
				       eIND_MES  = &eINDICE
				       eCORRECAO = eCORRECAO + (1000+((eIND_MES/eDIF_DIAS_MES*eDIF_DIAS_TOTAL)*1000/100)*eVLR_PARC)
				       SKIP
				    ENDDO
				    SELE 8
				    
                                    * Calculo do Juros 
                                    *=1008,06+((1/31*10)*1008,06/100)  Parcela corrigida+(juro 1%/dias do màs*dias de atraso)*parcela corrigida%)
			            eJUROS = eJUROS + (1000+((eTX_COMPER/eDIF_DIAS_MES*eDIF_DIAS_TOTAL)*1000/100)*(eVLR_PARC+eCORRECAO))
			            
			            * Calculo da Multa
			            *=1011,32+(1011,32*2/100)  Parcela corrigida e com juro+(parcela corrigida e com juro% * multa 2%
			            eMULTA = ((eVLR_PARC + eCORRECAO + eJUROS) * (eTX_MULTA / 100 ))
                                    
			            
			            * Preparaá∆o do quadro de Pagamento
			            eAUX_ACUM = eIND_MES
			            
                                 ELSE
                                    sele 8 
                                    * Pagamento fora do Mes
                                    * Data de Vencimento
                                    eTAM_DIA  = 0
                                    eTAM_MES  = 0
                                    eTAM_DIA  = LEN(eDIA_VENC)
                                    eTAM_MES  = LEN(eMES_VENC)

                                    eDIA_VENC = IIF(eTAM_DIA = 1,'0'+SUBSTR(eDIA_VENC,1,1),SUBSTR(eDIA_VENC,1,2))
                                    
                                    eMES_VENC = IIF(eTAM_MES = 1,'0'+SUBSTR(eMES_VENC,1,1),SUBSTR(eMES_VENC,1,2))
                                    
                                    eANO_VENC = SUBSTR(eANO_VENC,1,4)

                                    * Data de Pagamento
                                    eTAM_DIA  = LEN(eDIA_PAGO)
                                    eTAM_MES  = LEN(eMES_PAGO)

                                    eDIA_PAGO = IIF(eTAM_DIA = 1,'0'+SUBSTR(eDIA_PAGO,1,1),SUBSTR(eDIA_PAGO,1,2))
                                    
                                    eMES_PAGO = IIF(eTAM_MES = 1,'0'+SUBSTR(eMES_PAGO,1,1),SUBSTR(eMES_PAGO,1,2))
                                    
                                    eANO_PAGO = SUBSTR(eANO_PAGO,1,4)

                                    * Data Contagem 
                                    eTAM_DIA  = 0
                                    eTAM_MES  = 0
                                    eTAM_DIA  = LEN(eDIA_VENC)
                                    eTAM_MES  = LEN(eMES_VENC)

                                    eDIA_CONT = '01'
                                    IF eMES_VENC+1 = 13
                                       eMES_CONT = '01'
                                       eANO_CONT = SUBSTR(STR(VAL(eANO_VENC+1)),1,4)                                       
                                    ELSE   
                                       eMES_CONT = IIF(eTAM_MES = 1,'0'+SUBSTR(eMES_VENC+1,1,1),SUBSTR(eMES_VENC+1,1,2))
                                       eANO_CONT = SUBSTR(eANO_VENC,1,4)                                       
                                    ENDIF
                                    eDATA_CONT = CTOD(eDIA_CONT+'/'+eMES_CONT+'/'+eANO_CONT)
                                    eDATA_CONT = eDATA_CONT - 1
                                    eDIF_DIAS_MES = VAL(SUBSTR(DTOC(eDATA_CONT),1,2))                
                                    ?eDATA_CONT
                                    ?eDIF_DIAS_MES
                                    PPARA()
                                    
                                    * Calculo do Indice Acumulado
				    SELE 7
				    GOTO TOP
				    SET FILTER TO ((ANO+MES >= eANO_VENC+eMES_VENC) .AND. (ANO+MES <= eANO_PAGO+eMES_PAGO))
				    GOTO TOP
				    eIND_MES   = 1
				    
				    * Calculo da Correá∆o
				    *=1000+((2,5/31*10)*1000/100) Parcela+(°ndice/dias do màs*dias de atraso)*parcela%)
				    DO WHILE .NOT. EOF()
				       eIND_MES  = &eINDICE
				       eCORRECAO = eCORRECAO + (1000+((eIND_MES/eDIF_DIAS_MES*eDIF_DIAS_TOTAL)*1000/100)*eVLR_PARC)
				       SKIP
				    ENDDO
				    SELE 8
                                    * Calculo do Juros 
                                    *=1008,06+((1/31*10)*1008,06/100)  Parcela corrigida+(juro 1%/dias do màs*dias de atraso)*parcela corrigida%)
			            eJUROS = eJUROS + (1000+((eTX_COMPER/eDIF_DIAS_MES*eDIF_DIAS_TOTAL)*1000/100)*(eVLR_PARC+eCORRECAO))
			            
			            * Calculo da Multa
			            *=1011,32+(1011,32*2/100)  Parcela corrigida e com juro+(parcela corrigida e com juro% * multa 2%
			            eMULTA = ((eVLR_PARC + eCORRECAO + eJUROS) * (eTX_MULTA / 100 ))
                                    
			            
			            * Preparaá∆o do quadro de Pagamento
			            eAUX_ACUM = eIND_MES
			                
                                 ENDIF                                 

                           ENDCASE
                     ENDCASE
                     * Correá∆o
                     @ 15,12 SAY 'Correcao        (+): ' GET eCORRECAO PICT '@R 999,999,999.99'
                     @ 15,50 SAY 'Indice : ' GET eINDICE PICT '@S10'
                     @ 15,70 SAY '/'         GET eAUX_ACUM PICT '@R 999.99'
                     * Juros
                     @ 16,12 SAY 'Juros           (+): ' GET eJUROS    PICT '@R 999,999,999.99'
                     @ 16,50 SAY 'Taxa   : ' GET eTX_COMPER PICT '@R 999.99'
                     @ 16,67 SAY  ePERID_COMI
                     * Multa
                     @ 17,12 SAY 'Multa           (+): ' GET eMULTA    PICT '@R 999,999,999.99'
                     @ 17,50 SAY 'Taxa   : ' GET eTX_MULTA  PICT '@R 999.99'
                     
                     eVLR_PAGO = ((eVLR_PARC + (eMULTA + eJUROS + eCORRECAO)) - eDESCONTO)
                     eVLR_ANTE = eVLR_PAGO
                     @ 18,12 SAY 'Valor Pagamento    : ' GET eVLR_PAGO PICT '@R 999,999,999.99'
                     eTX_COMPER = eTX_COMPER / 100
                     eAUX       = eAUX - 1 
                     @ 18,50 SAY 'Mes/Dia: ' GET eAUX PICT '@R 999'
                     eDIAS = eDATA_PAGO - eDATA_VENC
                     @ 18,64 SAY '/'         GET eDIAS PICT '@R 99999'
                     CLEAR GETS
                     @ 19,12 SAY 'Desconto        (-): ' GET eDESCONTO PICT '@R 999,999,999.99'
                     READ
                     eVLR_PAGO = ((eVLR_PARC + (eMULTA + eJUROS + eCORRECAO)) - eDESCONTO)
                     @ 20,12 SAY 'Valor Pagamento    : ' GET eVLR_PAGO PICT '@R 999,999,999.99'
                     READ
                     IF eVLR_PAGO <> eVLR_ANTE
                        IF eVLR_PAGO < eVLR_ANTE
                           eDESCONTO = (eVLR_ANTE - eVLR_PAGO)
                        ELSE
                           eMULTA    = (eVLR_PAGO - eVLR_ANTE)
                        ENDIF
                     ENDIF
                     @ 20,50 SAY 'Conta Corrente: ' GET eCONTA    PICT '@R 999'
                     READ
                     eMOEDA = SPACE(1)
                     @ 21,37 SAY '(Boleto/Dinheiro/Cheque/Outros)'
                     @ 21,12 SAY 'Tipo(B/D/C/O)    : ' GET eMOEDA PICT 'X' VALID eMOEDA $('DCBO')
                     READ
                     eNUM_BANCO = SPACE(3)
                     eNUM_AGENC = SPACE(10)
                     eNUM_CONTA = SPACE(15)
                     eNUM_CHEQUE= SPACE(15)
                     IF eMOEDA = 'C'
                        @ 21,12 SAY REPLICATE(' ',60)
                        @ 21,12 SAY 'Banco/Ag/C.C/Cheque: ' GET eNUM_BANCO  PICT '999'
                        @ 21,37 SAY '/'                     GET eNUM_AGENC  PICT '9999999999'
                        @ 21,49 SAY '/'                     GET eNUM_CONTA  PICT '9999999999'
                        @ 21,61 SAY '/'                     GET eNUM_CHEQUE PICT '9999999999'
                        READ
                     ENDIF
                     DO PLQUA WITH '01'
                     DO PPERG WITH 'Confirma o Pagamento da Parcela (S/N) ? '
                     IF sisRESP = 'N'
                        SELE 8
                        SET CURSOR OFF
                        RESTORE SCREEN FROM TELA04
                        RETURN RET
                     ELSE
                        SELE 8
                        IF F_BLOQR()
                           REPLACE DATA_PGTO  WITH eDATA_PAGO,;
                                   VLR_PGTO   WITH eVLR_PAGO,;
                                   ORIGEM     WITH 'B'
                           REPLACE CONTA      WITH eCONTA,;
                                   NUM_BANCO  WITH eNUM_BANCO,;
                                   NUM_AGENC  WITH eNUM_AGENC
                           REPLACE NUM_CONTA  WITH eNUM_CONTA,;
                                   NUM_CHEQUE WITH eNUM_CHEQUE,;
                                   VLR_MULTA  WITH eMULTA
                           REPLACE VLR_CORREC WITH eCORRECAO,;
                                   VLR_COMISS WITH eJUROS,;
                                   VLR_DESCON WITH eDESCONTO
                           REPLACE MOEDA      WITH eMOEDA
                           eCONTA = eCONTA_ORI
                           UNLOCK
                           eNUM_PARC  = NUM_PARC
                           eTIPO_PARC = TIPO_PARC
                        ENDIF
                        F_LACESSO(sisUSUARIO,'VENDA DE PRODUTOS','PGTO.PARCELA : '+ALLTRIM(STR(pCLASSE,5))+'/'+ALLTRIM(pQUADRA)+'/'+ALLTRIM(pUNIDADE)+'/'+ALLTRIM(STR(pPARTE,1))+' PARC : '+ALLTRIM(eNUM_PARC)+' TIPO : '+ALLTRIM(TIPO_PARC))
                        SELE 8
                        DO PLQUA WITH '01'
                        DO PPERG WITH 'Confirma a Emiss∆o do Recibo (S/N) ? '
                        IF sisRESP = 'S'
                           DO PLQUA WITH '01'
                           eRECIBO = 0
                           @ 23,00 SAY 'Informe a Quantidade de Vias de 1 a 3 ? ' GET eRECIBO PICT '99999' RANGE 1,3
                           READ
                           SELE 9
                           USE STATUS
                           GOTO TOP
                           eNUM_RECI = 0
                           eNUM_RECI = NUM_RECI
                           eVEZES  = 0
                           DO PMENSAG WITH 'Posicione o Formulario 80 Colunas e Tecle [ENTER]'
                           DO PPARA
                           DO PIMPR
                           SET DEVICE TO PRINT
                           LIN = PROW()
                           @ LIN,00 SAY ' '
                           ePAG = 1
                           DO WHILE eVEZES < eRECIBO
                              @ LIN,00  SAY sisLETRAC+'Empresa : '+sisTIT1
                              @ LIN,120 SAY 'Data : '+dtoc(DATE())
                              LIN = LIN + 1
                              @ LIN,30  SAY sisLETRAE+'RECIBO PAGAMENTO DE PARCELAS No. '+ALLTRIM(STR(INT(eNUM_RECI)))+sisLETRAC
                              LIN = LIN + 1
                              @ LIN,00  SAY REPLICATE('_',135)
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              SELE 2
                              SEEK pCLASSE
                              @ LIN,10  SAY '   Produto     : '+ALLTRIM(STR(pCLASSE,5))+' - '+ALLTRIM(DESCRICAO)
                              IF eTIPO_PRO = 1
                                 @ LIN,60  SAY sisGRUPO1+'  : '+ALLTRIM(pQUADRA)
                                 @ LIN,80  SAY sisUNID1+'  : '+ALLTRIM(pUNIDADE)
                                 @ LIN,105 SAY sisUNID2+'  : '+STR(pPARTE,1)
                              ELSE
                                 SELE 4
                                 @ LIN,60  SAY sisGRUPO2+'    : '+ALLTRIM(pQUADRA)+' - '+DESCRICAO
                                 @ LIN,105 SAY sisUNID2+'    : '+ALLTRIM(pUNIDADE)
                              ENDIF
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              eTAM_CLIE = LEN(ALLTRIM(eCLIENTE))
                              eCLIENTE  = ALLTRIM(eCLIENTE)+REPLICATE('*',50-eTAM_CLIE)
                              IF eSEXO = 'M'
                                 @ LIN,20  SAY 'Recebi do Sr. '+eCLIENTE+' a  importancia  de '
                              ELSE
                                 IF eSEXO = 'F'
                                    @ LIN,20  SAY 'Recebi da Sra. '+eCLIENTE+' a  importancia  de '
                                 ELSE
                                    @ LIN,20  SAY 'Recebi  de  '+eCLIENTE+' a  importancia  de '
                                 ENDIF
                              ENDIF
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              SELE 8
                              @ LIN,20 SAY sisMOEDA
                              @ LIN,34 SAY VLR_PGTO PICT '@R 999,999,999.99'
                              @ LIN,49 SAY ', Referente a pagamento da parcela No. '+ALLTRIM(NUM_PARC)+' com vencimento'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              eDT_VENC = DTOC(DATA_VENC)
                              DO PDATAEXT WITH eDT_VENC
                              @ LIN,20 SAY 'para '+sisEXT+'.'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              SELE 8
                              @ LIN,00 SAY '|'
                              @ LIN,10 SAY 'Valor da Prestacao : '
                              @ LIN,30 SAY VLR_PARC   PICT '@R 999,999,999,999.99'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,10 SAY 'Multa/Juros/C.M.(+): '
                              eDIFERENCA = eMULTA + eCORRECAO + eJUROS
                              @ LIN,30 SAY eDIFERENCA PICT '@R 999,999,999,999.99'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,10 SAY 'Desconto (-)       : '
                              eDIFERENCA = eDESCONTO
                              @ LIN,30 SAY eDIFERENCA PICT '@R 999,999,999,999.99'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,10 SAY 'Valor Pagamento    : '
                              @ LIN,30 SAY VLR_PGTO   PICT '@R 999,999,999,999.99'
                              DO CASE
                                 CASE MOEDA = 'D'
                                    @ LIN,60 SAY 'PGTO : DINHEIRO'
                                 CASE MOEDA = 'C'
                                    @ LIN,60 SAY 'PGTO : CHEQUE'
                                 CASE MOEDA = 'B'
                                    @ LIN,60 SAY 'PGTO : BANCO'
                                 CASE MOEDA = 'O'
                                    @ LIN,60 SAY 'PGTO : OUTROS'
                                 CASE MOEDA = ' '
                                    @ LIN,60 SAY 'PGTO : INDEFINIDO'
                              ENDCASE
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,70  SAY '----------------------------------------------'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              eDT_PAGO = DTOC(eDATA_PAGO)
                              DO PDATAEXT WITH eDT_PAGO
                              @ LIN,10  SAY ALLTRIM(eCIDADE)+', '+sisEXT
                              @ LIN,70  SAY '      '+sisTIT1
                              @ LIN,70  SAY '      '+sisTIT1
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,76  SAY 'CGC  :' 
                              @ LIN,82  SAY eCPF_CGC PICT '@R 99.999.999/9999-99'
                              @ LIN,76  SAY 'CGC  :' 
                              @ LIN,82  SAY eCPF_CGC PICT '@R 99.999.999/9999-99'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              IF eNUM_BANCO = SPACE(3)
                                 @ LIN,10  SAY 'Banco : ________________________ No. Cheque : ________________________ Observacao : ________________________'
                              ELSE
                                 @ LIN,10 SAY 'Banco/Ag/C.C/Cheque: '+eNUM_BANCO+'/'+ALLTRIM(eNUM_AGENC)+'/'+ALLTRIM(eNUM_CONTA)+'/'+ALLTRIM(eNUM_CHEQUE)
                              ENDIF
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY REPLICATE('_',135)
                              LIN = LIN + 1
                              @ LIN,08  SAY REPLICATE('- ',60)
                              LIN = LIN + 2
                              @ LIN,08  SAY ' '
                              eVEZES = eVEZES + 1
                           ENDDO
                           SET DEVICE TO SCREEN
                           DO PLQUA WITH '01'
                           DO PPERG WITH 'Deseja Avancar a folha (S/N) ? '
                           IF sisRESP = 'S'
                              SET DEVICE TO PRINT
                              @ 00,00 SAY ' '
                              LIN  = 1
                              ePAG = 1
                              SET DEVICE TO SCREEN
                           ENDIF
                           SELE 8
                           IF F_BLOQR()
                              REPLACE COD_BANCO WITH STR(eNUM_RECI,10,0)
                              UNLOCK
                           ENDIF
                           SELE 9
                           eNUM_RECI = eNUM_RECI + 1
                           IF F_BLOQR()
                              REPLACE NUM_RECI WITH eNUM_RECI
                              UNLOCK
                           ENDIF
                           SELE 8
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
            SET CURSOR OFF
            RESTORE SCREEN FROM TELA04
         ELSE
           IF F_BLOQR()
              SAVE SCREEN TO TELA04
              SET CURSOR ON
              FLD_NAME = CAMPO[DB_COLUMN]
              FLD_PIC  = PIC[DB_COLUMN]
              @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
              READ
              SET CURSOR OFF
              RESTORE SCREEN FROM TELA04
              UNLOCK
           ENDIF
	 ENDIF
      CASE KEY == 32
         * Calculo dos Atrazados
         SAVE SCREEN TO TELA04
         SET CURSOR ON
         DO PLQUA WITH '01'
         DO PPERG WITH 'Calculo dos Atrazados (S/N) ? '
         IF sisRESP = 'S'
            * Selecao do Periodo
            eDATA_INI   = CTOD('')
            eDATA_FIM   = CTOD('')
            DO WHILE .T.
               DO PLQUA WITH '01'
               @ 23,02 SAY 'Periodo : ' GET eDATA_INI
               @ 23,25 SAY 'Ate : '     GET eDATA_FIM
               READ
               IF eDATA_INI = CTOD('') .OR. eDATA_FIM = CTOD('')
                  DO PLQUA WITH '01'
                  DO PMSGR WITH 'Datas Inv†lidas !',3
                  LOOP
               ELSE
                  IF eDATA_INI > eDATA_FIM
                     DO PLQUA WITH '01'
                     DO PMSGR WITH 'Data Inicio > Que Final !',3
                     LOOP
                  ELSE
                     EXIT
                  ENDIF
               ENDIF
            ENDDO
            eTOTAL    = 0
            eREGISTRO = 0
            ePARCELAS = 0
            eREGISTRO = RECNO()
            SELE 8
            SET FILTER TO CLASSE = STR(pCLASSE,5) .AND. QUADRA = pQUADRA .AND. NUM_IMOVEL = pUNIDADE .AND. PARTE = pPARTE
            GOTO TOP
            DO WHILE .NOT. EOF()
               IF (DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .AND. DATA_PGTO = CTOD('')
                  ePERID_COMI= PERID_COMI
                  eTX_MULTA  = MULTA/100
                  eTX_COMPER = COMISSAO/100
                  eVLR_PARC  = VLR_PARC
                  eDATA_VENC = DATA_VENC
                  eVLR_PAGO  = 0
                  eDATA_PAGO = DATE()
                  eMULTA     = 0
                  eJUROS     = 0
                  eDESCONTO  = 0
                  eAUX       = 0

                  CALC_PAG2()
                  SELE 8
                  ePARCELAS = ePARCELAS + 1

                  eTOTAL = eTOTAL + ((eVLR_PARC + (eMULTA + eJUROS + eCORRECAO)) - eDESCONTO)

                  DO PLQUA WITH '01'
                  @ 23,00 SAY 'Total da Divida Corrigida : '
                  @ 23,27 SAY eTOTAL PICT '@R 999,999,999,999.99'
                  @ 23,60 SAY ePARCELAS PICT '@R 999'
               ENDIF
               SKIP
            ENDDO
            DO PLQUA WITH '01'
            @ 23,00 SAY 'Total da Divida Corrigida : '
            @ 23,27 SAY eTOTAL PICT '@R 999,999,999,999.99'
            @ 23,60 SAY ePARCELAS PICT '@R 999'
            DO PPARA
            GOTO eREGISTRO
         ENDIF
         SET CURSOR OFF
         RESTORE SCREEN FROM TELA04

      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
************************ funcao *********************************
FUNCTION FUNC_PROPR3
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
	 DO CASE
	    CASE DB_COLUMN = 5
	       SAVE SCREEN TO T_PROP1
	       ePROPRIET_1 = PROPRIET_1
	       SELE 5
	       SEEK ePROPRIET_1
	       @ 12,13 CLEAR TO 14,67
	       @ 12,13 TO       14,67 DOUBLE
	       DO PCENTRA WITH 'Nome do Proprietario No. 1',12
	       @ 13,15 GET CLIENTE
	       CLEAR GETS
	       SELE 1
	       DO PPARA
	       RESTORE SCREEN FROM T_PROP1
	    CASE DB_COLUMN = 6
	       SAVE SCREEN TO T_PROC1
               @ 09,01 CLEAR TO 21,79
               @ 09,01 TO       21,79
               DO PCENTRA WITH ' Observacoes Sobre o Processo de Cobranáa ',09
	       IF F_BLOQR()
                  SET CURSOR ON
                  eOLD = S1->OBS_PROC_1
                  eNEW = MEMOEDIT(eOLD,10,02,20,78,.T.,"F_MEDITE2")
                  REPLACE OBS_PROC_1 WITH eNEW
                  SET CURSOR OFF
                  UNLOCK
	       ENDIF
	       DO PPARA
	       RESTORE SCREEN FROM T_PROC1
	    CASE DB_COLUMN = 7
	       SAVE SCREEN TO T_PROP2
	       ePROPRIET_2 = PROPRIET_2
	       SELE 5
	       SEEK ePROPRIET_2
	       @ 12,13 CLEAR TO 14,67
	       @ 12,13 TO       14,67 DOUBLE
	       DO PCENTRA WITH 'Nome do Proprietario No. 2',12
	       @ 13,15 GET CLIENTE
	       CLEAR GETS
	       SELE 1
	       DO PPARA
	       RESTORE SCREEN FROM T_PROP2
	    CASE DB_COLUMN = 8
	       SAVE SCREEN TO T_PROC2
               @ 09,01 CLEAR TO 21,79
               @ 09,01 TO       21,79
               DO PCENTRA WITH ' Observacoes Sobre o Processo de Cobranáa ',09       
	       IF F_BLOQR()
                  SET CURSOR ON
                  eOLD = S1->OBS_PROC_2
                  eNEW = MEMOEDIT(eOLD,10,02,20,78,.T.,"F_MEDITE2")
                  REPLACE OBS_PROC_2 WITH eNEW
                  SET CURSOR OFF
		  UNLOCK
	       ENDIF
	       DO PPARA
	       RESTORE SCREEN FROM T_PROC2
	    OTHERWISE
	       RET = 0
	 ENDCASE
      ENDCASE
ENDIF
RETURN RET
*************************  fim  *********************************
************************ funcao *********************************
FUNCTION F_MEDITE2
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
************************ funcao *********************************
FUNCTION FUNC_PAG2_TS
PARAMETERS DB_MODE,DB_COLUMN
PRIVATE RET,KEY
KEY = LASTKEY()
RET = 1
IF DB_MODE == 4
   DO CASE
      CASE KEY == 13
         IF DB_COLUMN <= 8
            SAVE SCREEN TO TELA04
            IF F_BLOQR()
               SET CURSOR ON
               DO PLQUA WITH '01'
               DO PPERG WITH 'Pagamento de Parcela (S/N) ? '
               IF sisRESP = 'S'
                  IF .NOT. ATIVA
                     DO PLQUA WITH '01'
                     DO PMSGR WITH 'Atená∆o !',3
                     DO PMENSAG WITH 'N∆o Ç Possivel Pagar uma Parcela Inativa !'
                     DO PPARA
                  ELSE
                     eAUX_ACUM  = 0
                     ePERID_COMI= PERID_COMI
                     eTX_MULTA  = MULTA/100
                     eTX_COMPER = COMISSAO/100
                     eVLR_PARC  = VLR_PARC
                     eDATA_VENC = DATA_VENC
                     eVLR_PAGO  = 0
                     eDATA_PAGO = DATE()
                     eMULTA     = 0
                     eJUROS     = 0
                     eDESCONTO  = 0
                     eAUX       = 0
                     @ 10,02 CLEAR TO 22,78
                     @ 10,02 TO 22,78 DOUBLE
                     DO PCENTRA WITH "VALORES DA PARCELA",11
                     @ 12,12 SAY 'Valor da Parcela   : ' GET eVLR_PARC PICT '@R 999,999,999.99'
                     @ 13,12 SAY 'Data de Vencimento : ' GET eDATA_VENC
                     CLEAR GETS
                     @ 14,12 SAY 'Data de Pagamento  : ' GET eDATA_PAGO
                     READ
                     DO CASE
                        CASE eDATA_VENC = eDATA_PAGO
                           DO PLQUA WITH '01'
                           DO PMENSAG WITH 'Pagamento dentro do Prazo, Aguarde ... 1'
                           eDESCONTO = 0
                           eMULTA    = 0
                           eJUROS    = 0
                           eCORRECAO = 0
                        CASE eDATA_VENC > eDATA_PAGO
                           * Calculo de Desconto de Parcelas
                           DO PLQUA WITH '01'
                           DO PMENSAG WITH 'Pagamento Antecipado, Aguarde ... 2'
                           eDIA_VENC = 0
                           eMES_VENC = 0
                           eANO_VENC = 0
                           eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
                           eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
                           eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
                           eDIA_VENC = VAL(eDIA_VENC)
                           eMES_VENC = VAL(eMES_VENC) - 1
                           eANO_VENC = VAL(eANO_VENC) 
                           IF eMES_VENC = 0
                              eMES_VENC = 12
                              eANO_VENC = eANO_VENC - 1
                           ENDIF
                           eDIA_VECC = eDIA_VENC
                           eMES_VECC = eMES_VENC
                           eANO_VECC = eANO_VENC
                           eTAM_DIA  = LENNUM(INT(eDIA_VENC))
                           eTAM_MES  = LENNUM(INT(eMES_VENC))
                           eDIA_VENC = IIF(eTAM_DIA = 1,'0'+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
                           eMES_VENC = IIF(eTAM_MES = 1,'0'+STR(eMES_VENC,1),STR(eMES_VENC,2))
                           eANO_VENC = STR(INT(eANO_VENC),4)
                           eMES_ANT  = CTOD(eDIA_VENC+'/'+eMES_VENC+'/'+eANO_VENC)
                           DO CASE
                              CASE eDATA_VENC > eDATA_PAGO .AND. eDATA_PAGO < eMES_ANT
                                 DO PLQUA WITH '01'
                                 DO PMENSAG WITH 'Calculo de Desconto, Aguarde ...'
                                 eMULTA    = 0
                                 eJUROS    = 0
                                 eCORRECAO = 0
                                 eAUX      = 1
                                 eDIA_VENC = 0
                                 eMES_VENC = 0
                                 eANO_VENC = 0
                                 eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
                                 eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
                                 eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
                                 eDIA_PAGO = 0
                                 eMES_PAGO = 0
                                 eANO_PAGO = 0
                                 eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
                                 eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
                                 eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
                                 eAUX_VENC = CTOD('')
                                 eDIA_VENC = VAL(eDIA_VENC)
                                 eMES_VENC = VAL(eMES_VENC)
                                 eANO_VENC = VAL(eANO_VENC)
                                 DO WHILE .T.
                                    eMES_VENC = eMES_VENC - 1
                                    IF eMES_VENC = 0
                                       eMES_VENC = 12
                                       eANO_VENC = eANO_VENC - 1
                                    ENDIF
                                    eAUX      = eAUX + 1
                                    eTAM_DIA  = LENNUM(INT(eDIA_VENC))
                                    eTAM_MES  = LENNUM(INT(eMES_VENC))
                                    eDIA_VENC = IIF(eDIA_VENC > 28 .AND. eMES_VENC = 2,28,eDIA_VENC)
                                    eDIA_VENC = IIF(eTAM_DIA = 1,'0'+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
                                    eMES_VENC = IIF(eTAM_MES = 1,'0'+STR(eMES_VENC,1),STR(eMES_VENC,2))
                                    eANO_VENC = STR(INT(eANO_VENC),4)
                                    eAUX_VENC = CTOD(eDIA_VENC+'/'+eMES_VENC+'/'+eANO_VENC)
                                    eDIA_VENC = VAL(IIF(eTAM_DIA = 1,'0'+ALLTRIM(eDIA_VENC),ALLTRIM(eDIA_VENC)))
                                    eMES_VENC = VAL(IIF(eTAM_MES = 1,'0'+ALLTRIM(eMES_VENC),ALLTRIM(eMES_VENC)))
                                    eANO_VENC = VAL(eANO_VENC)
                                    IF MONTH(eAUX_VENC) = MONTH(eDATA_PAGO) .AND. YEAR(eAUX_VENC) = YEAR(eDATA_PAGO)
                                       EXIT
                                    ENDIF
                                 ENDDO
                                 IF eAUX_VENC < eDATA_PAGO
                                    eAUX = eAUX - 1
                                 ENDIF
                                 * Calculo do desconto
                                 eDESCONTO = (eVLR_PARC * (eTX_DESCON * eAUX))
                                 eMULTA    = 0
                                 eJUROS    = 0
                                 eCORRECAO = 0
                              OTHERWISE
                                 eDESCONTO = 0
                                 eMULTA    = 0
                                 eJUROS    = 0
                                 eCORRECAO = 0
                           ENDCASE
                        OTHERWISE
                           * Calculo de Multa e Juros
                           DO CASE

                              CASE eDATA_PAGO > eDATA_VENC  .AND. ((eDATA_PAGO - eDATA_VENC) >= 30)
                                 DO PLQUA WITH '01'
                                 DO PMENSAG WITH 'Calculo de Multa e Juros, Aguarde ...3'
                                 eDESCONTO = 0
                                 eMULTA    = 0
                                 eJUROS    = 0
                                 eCORRECAO = 0
                                 eAUX      = 1
                                 eDIA_VENC = 0
                                 eMES_VENC = 0
                                 eANO_VENC = 0
                                 eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
                                 eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
                                 eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
                                 eDIA_VENO = eDIA_VENC
                                 eMES_VENO = eMES_VENC
                                 eANO_VENO = eANO_VENC
                                 eDIA_PAGO = 0
                                 eMES_PAGO = 0
                                 eANO_PAGO = 0
                                 eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
                                 eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
                                 eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
                                 eAUX_VENC = CTOD('')
                                 eDIA_VECC = VAL(eDIA_VENC)
                                 eMES_VECC = VAL(eMES_VENC)
                                 eANO_VECC = VAL(eANO_VENC)
                                 eMES_VECC = eMES_VECC + 1
                                 eDIF_DIAS = VAL(eDIA_PAGO)
                                 IF eMES_VECC = 13
                                    eMES_VECC = 01
                                    eANO_VECC = eANO_VECC + 1
                                 ENDIF
                                 eTAM_MES  = LENNUM(INT(eMES_VECC))
                                 eMES_VECC = IIF(eTAM_MES = 1,'0'+STR(eMES_VECC,1),STR(eMES_VECC,2))
                                 eANO_VECC = STR(INT(eANO_VECC),4)
                                 DO WHILE .T.
                                    eDIA_VENC = VAL(eDIA_VENC)
                                    eMES_VENC = VAL(eMES_VENC)
                                    eANO_VENC = VAL(eANO_VENC)
                                    eMES_VENC = eMES_VENC + 1
                                    IF eMES_VENC = 13
                                       eMES_VENC = 01
                                       eANO_VENC = eANO_VENC + 1
                                    ENDIF
                                    eAUX = eAUX + 1
                                    eTAM_DIA  = LENNUM(INT(eDIA_VENC))
                                    eTAM_MES  = LENNUM(INT(eMES_VENC))
                                    eDIA_VENC = IIF(eDIA_VENC > 28 .AND. eMES_VENC = 2,28,eDIA_VENC)
                                    eDIA_VENC = IIF(eTAM_DIA = 1,'0'+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
                                    eMES_VENC = IIF(eTAM_MES = 1,'0'+STR(eMES_VENC,1),STR(eMES_VENC,2))
                                    eANO_VENC = STR(INT(eANO_VENC),4)
                                    eAUX_VENC = CTOD(eDIA_VENC+'/'+eMES_VENC+'/'+eANO_VENC)
                                    IF (MONTH(eAUX_VENC) = MONTH(eDATA_PAGO)) .AND. (YEAR(eAUX_VENC) = YEAR(eDATA_PAGO))
                                       EXIT
                                    ENDIF
                                 ENDDO
                                 IF eAUX_VENC > eDATA_PAGO
                                    eAUX = eAUX - 1
                                 ENDIF
                                 SELE 7
                                 GOTO TOP
                                 SET FILTER TO ((ANO+MES >= eANO_VECC+eMES_VECC) .AND. (ANO+MES <= eANO_PAGO+eMES_PAGO))
                                 GOTO TOP
                                 eIND_ACUM  = 1
                                 DO WHILE (ANO+MES <= eANO_PAGO+eMES_PAGO) .AND. .NOT. EOF()
                                    eIND_ACUM = ((&eINDICE/100) + 1)
                                    eCORRECAO = eCORRECAO + (eVLR_PARC * (eIND_ACUM - 1))
                                    SKIP
                                 ENDDO
                                 eAUX_ACUM = eIND_ACUM
                                 eAUX2_ACUM = 0

                                 IF eDIF_DIAS > 0 .AND. ePRO_RATA
                                    SELE 7
                                    GOTO TOP
                                    SET FILTER TO (ANO+MES = eANO_VENO+eMES_VENO)
                                    GOTO TOP
                                    eIND_ACUM = ((&eINDICE/100) + 1)
                                    eAUX2_ACUM = ( ( ( (eIND_ACUM) ^ ( 1 / 30 )) ^ eDIF_DIAS) - 1)
                                    eCORRECAO = eCORRECAO + (eVLR_PARC * eAUX2_ACUM)
                                 ENDIF
                                 eAUX_ACUM = eAUX_ACUM * (eAUX2_ACUM + 1)

                                 IF eANATOCISMO
                                    IF ePERID_COMI = 'a.m.'
                                       eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eAUX - 1)) - 1)
                                    ELSE
                                       eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eDATA_PAGO - eDATA_VENC)) - 1)
                                    ENDIF
                                 ELSE
                                    IF ePERID_COMI = 'a.m.'
                                       eJUROS    = (eVLR_PARC + eCORRECAO) * ((eTX_COMPER) * (eAUX - 1))
                                    ELSE
                                       eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eDATA_PAGO - eDATA_VENC)) - 1)
                                    ENDIF
                                 ENDIF
                                 * Calculo do Multa e Juros

							     * ALTERAÄ«O PARA TERRA SIM«O
								 
                                 eMULTA    = (eVLR_PARC + eCORRECAO + eJUROS ) * eTX_MULTA
                                 eDESCONTO = 0

                              CASE eDATA_PAGO > eDATA_VENC .AND. ((eDATA_PAGO - eDATA_VENC) < 30)
                                 DO PLQUA WITH '01'
                                 DO PMENSAG WITH 'Calculo de Multa, Aguarde ...4'
                                 eDESCONTO = 0
                                 eMULTA    = 0
                                 eJUROS    = 0
                                 eCORRECAO = 0
                                 eAUX      = 1
                                 eDIA_VENC = 0
                                 eMES_VENC = 0
                                 eANO_VENC = 0
                                 eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
                                 eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
                                 eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
                                 eDIA_VENO = eDIA_VENC
                                 eMES_VENO = eMES_VENC
                                 eANO_VENO = eANO_VENC
                                 eDIA_PAGO = 0
                                 eMES_PAGO = 0
                                 eANO_PAGO = 0
                                 eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
                                 eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
                                 eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
                                 eAUX_VENC = CTOD('')
                                 eDIA_VECC = VAL(eDIA_VENC)
                                 eMES_VECC = VAL(eMES_VENC)
                                 eANO_VECC = VAL(eANO_VENC)
                                 eMES_VECC = eMES_VECC + 1
                                 eDIF_DIAS = 30 - VAL(eDIA_PAGO)
                                 IF eMES_VECC = 13
                                    eMES_VECC = 01
                                    eANO_VECC = eANO_VECC + 1
                                 ENDIF
                                 eTAM_MES  = LENNUM(INT(eMES_VECC))
                                 eMES_VECC = IIF(eTAM_MES = 1,'0'+STR(eMES_VECC,1),STR(eMES_VECC,2))
                                 eANO_VECC = STR(INT(eANO_VECC),4)
                                 IF eAUX_VENC >= eDATA_PAGO
                                    eAUX = eAUX - 1
                                 ENDIF

                                 * Calculo de Correcao
                                 IF eDIF_DIAS > 0  .AND. ePRO_RATA
                                    SELE 7
                                    GOTO TOP
                                    SET FILTER TO (ANO+MES = eANO_VENO+eMES_VENO)
                                    GOTO TOP
                                    eIND_ACUM = ((&eINDICE/100) + 1)
                                    eCORRECAO = eCORRECAO + (eVLR_PARC * ((eIND_ACUM ^ (( 1000 / 30 ) / 1000) ^ eDIF_DIAS) - 1))
                                 ENDIF

                                 eDESCONTO = 0
                                 IF eANATOCISMO
                                    IF ePERID_COMI = 'a.m.'
                                       eJUROS    = 0
                                    ELSE
                                       eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eDATA_PAGO - eDATA_VENC)) - 1)
                                    ENDIF
                                 ELSE
                                    IF ePERID_COMI = 'a.m.'
                                       eJUROS    = 0
                                    ELSE
                                       eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eDATA_PAGO - eDATA_VENC)) - 1)
                                    ENDIF
                                 ENDIF
                                 eMULTA    = (eVLR_PARC + eCORRECAO + eJUROS ) * eTX_MULTA

                           ENDCASE
                     ENDCASE
                     @ 15,12 SAY 'Correcao        (+): ' GET eCORRECAO PICT '@R 999,999,999.99'
                     @ 15,50 SAY 'Indice : ' GET eINDICE PICT '@S10'
                     eAUX_ACUM = ((eAUX_ACUM - 1) * 100)

                     @ 15,70 SAY '/'         GET eAUX_ACUM PICT '@R 99.999'
                     @ 16,12 SAY 'Multa           (+): ' GET eMULTA    PICT '@R 999,999,999.99'
                     eTX_MULTA = eTX_MULTA * 100

                     @ 16,50 SAY 'Taxa   : ' GET eTX_MULTA  PICT '@R 999.99'
                     eTX_MULTA = eTX_MULTA / 100

                     @ 17,12 SAY 'Juros           (+): ' GET eJUROS    PICT '@R 999,999,999.99'
                     eTX_COMPER = eTX_COMPER * 100 

                     @ 17,50 SAY 'Taxa   : ' GET eTX_COMPER PICT '@R 999.99'
                     @ 17,67 SAY  ePERID_COMI
                     eVLR_PAGO = ((eVLR_PARC + (eMULTA + eJUROS + eCORRECAO)) - eDESCONTO)
                     eVLR_ANTE = eVLR_PAGO

                     @ 18,12 SAY 'Valor Pagamento    : ' GET eVLR_PAGO PICT '@R 999,999,999.99'
                     eTX_COMPER = eTX_COMPER / 100
                     eAUX       = eAUX - 1 

                     @ 18,50 SAY 'Mes/Dia: ' GET eAUX PICT '@R 999'
                     eDIAS = eDATA_PAGO - eDATA_VENC

                     @ 18,64 SAY '/'         GET eDIAS PICT '@R 99999'
                     CLEAR GETS
                     @ 19,12 SAY 'Desconto        (-): ' GET eDESCONTO PICT '@R 999,999,999.99'
                     READ

                     eVLR_PAGO = ((eVLR_PARC + (eMULTA + eJUROS + eCORRECAO)) - eDESCONTO)
                     @ 20,12 SAY 'Valor Pagamento    : ' GET eVLR_PAGO PICT '@R 999,999,999.99'
                     READ

                     IF eVLR_PAGO <> eVLR_ANTE
                        IF eVLR_PAGO < eVLR_ANTE
                           eDESCONTO = (eVLR_ANTE - eVLR_PAGO)
                        ELSE
                           eMULTA    = (eVLR_PAGO - eVLR_ANTE)
                        ENDIF
                     ENDIF

                     @ 20,50 SAY 'Conta Corrente: ' GET eCONTA    PICT '@R 999'
                     READ
                     eMOEDA = SPACE(1)
                     @ 21,37 SAY '(Boleto/Dinheiro/Cheque/Outros)'
                     @ 21,12 SAY 'Tipo(B/D/C/O)    : ' GET eMOEDA PICT 'X' VALID eMOEDA $('DCBO')
                     READ
                     eNUM_BANCO = SPACE(3)
                     eNUM_AGENC = SPACE(10)
                     eNUM_CONTA = SPACE(15)
                     eNUM_CHEQUE= SPACE(15)
                     IF eMOEDA = 'C'
                        @ 21,12 SAY REPLICATE(' ',60)
                        @ 21,12 SAY 'Banco/Ag/C.C/Cheque: ' GET eNUM_BANCO  PICT '999'
                        @ 21,37 SAY '/'                     GET eNUM_AGENC  PICT '9999999999'
                        @ 21,49 SAY '/'                     GET eNUM_CONTA  PICT '9999999999'
                        @ 21,61 SAY '/'                     GET eNUM_CHEQUE PICT '9999999999'
                        READ
                     ENDIF
                     DO PLQUA WITH '01'
                     DO PPERG WITH 'Confirma o Pagamento da Parcela (S/N) ? '
                     IF sisRESP = 'N'
                        SELE 8
                        SET CURSOR OFF
                        RESTORE SCREEN FROM TELA04
                        RETURN RET
                     ELSE
                        SELE 8
                        IF F_BLOQR()
                           REPLACE DATA_PGTO  WITH eDATA_PAGO,;
                                   VLR_PGTO   WITH eVLR_PAGO,;
                                   ORIGEM     WITH 'B'
                           REPLACE CONTA      WITH eCONTA,;
                                   NUM_BANCO  WITH eNUM_BANCO,;
                                   NUM_AGENC  WITH eNUM_AGENC
                           REPLACE NUM_CONTA  WITH eNUM_CONTA,;
                                   NUM_CHEQUE WITH eNUM_CHEQUE,;
                                   VLR_MULTA  WITH eMULTA
                           REPLACE VLR_CORREC WITH eCORRECAO,;
                                   VLR_COMISS WITH eJUROS,;
                                   VLR_DESCON WITH eDESCONTO
                           REPLACE MOEDA      WITH eMOEDA
                           eCONTA = eCONTA_ORI
                           UNLOCK
                           eNUM_PARC  = NUM_PARC
                           eTIPO_PARC = TIPO_PARC
                        ENDIF
                        F_LACESSO(sisUSUARIO,'VENDA DE PRODUTOS','PGTO.PARCELA : '+ALLTRIM(STR(pCLASSE,5))+'/'+ALLTRIM(pQUADRA)+'/'+ALLTRIM(pUNIDADE)+'/'+ALLTRIM(STR(pPARTE,1))+' PARC : '+ALLTRIM(eNUM_PARC)+' TIPO : '+ALLTRIM(TIPO_PARC))
                        SELE 8
                        DO PLQUA WITH '01'
                        DO PPERG WITH 'Confirma a Emiss∆o do Recibo (S/N) ? '
                        IF sisRESP = 'S'
                           DO PLQUA WITH '01'
                           eRECIBO = 0
                           @ 23,00 SAY 'Informe a Quantidade de Vias de 1 a 3 ? ' GET eRECIBO PICT '99999' RANGE 1,3
                           READ
                           SELE 9
                           USE STATUS
                           GOTO TOP
                           eNUM_RECI = 0
                           eNUM_RECI = NUM_RECI
                           eVEZES  = 0
                           DO PMENSAG WITH 'Posicione o Formulario 80 Colunas e Tecle [ENTER]'
                           DO PPARA
                           DO PIMPR
                           SET DEVICE TO PRINT
                           LIN = PROW()
                           @ LIN,00 SAY ' '
                           ePAG = 1
                           DO WHILE eVEZES < eRECIBO
                              @ LIN,00  SAY sisLETRAC+'Empresa : '+sisTIT1
                              @ LIN,120 SAY 'Data : '+dtoc(DATE())
                              LIN = LIN + 1
                              @ LIN,30  SAY sisLETRAE+'RECIBO PAGAMENTO DE PARCELAS No. '+ALLTRIM(STR(INT(eNUM_RECI)))+sisLETRAC
                              LIN = LIN + 1
                              @ LIN,00  SAY REPLICATE('_',135)
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              SELE 2
                              SEEK pCLASSE
                              @ LIN,10  SAY '   Produto     : '+ALLTRIM(STR(pCLASSE,5))+' - '+ALLTRIM(DESCRICAO)
                              IF eTIPO_PRO = 1
                                 @ LIN,60  SAY sisGRUPO1+'  : '+ALLTRIM(pQUADRA)
                                 @ LIN,80  SAY sisUNID1+'  : '+ALLTRIM(pUNIDADE)
                                 @ LIN,105 SAY sisUNID2+'  : '+STR(pPARTE,1)
                              ELSE
                                 SELE 4
                                 @ LIN,60  SAY sisGRUPO2+'    : '+ALLTRIM(pQUADRA)+' - '+DESCRICAO
                                 @ LIN,105 SAY sisUNID2+'    : '+ALLTRIM(pUNIDADE)
                              ENDIF
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              eTAM_CLIE = LEN(ALLTRIM(eCLIENTE))
                              eCLIENTE  = ALLTRIM(eCLIENTE)+REPLICATE('*',50-eTAM_CLIE)
                              IF eSEXO = 'M'
                                 @ LIN,20  SAY 'Recebi do Sr. '+eCLIENTE+' a  importancia  de '
                              ELSE
                                 IF eSEXO = 'F'
                                    @ LIN,20  SAY 'Recebi da Sra. '+eCLIENTE+' a  importancia  de '
                                 ELSE
                                    @ LIN,20  SAY 'Recebi  de  '+eCLIENTE+' a  importancia  de '
                                 ENDIF
                              ENDIF
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              SELE 8
                              @ LIN,20 SAY sisMOEDA
                              @ LIN,34 SAY VLR_PGTO PICT '@R 999,999,999.99'
                              @ LIN,49 SAY ', Referente a pagamento da parcela No. '+ALLTRIM(NUM_PARC)+' com vencimento'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              eDT_VENC = DTOC(DATA_VENC)
                              DO PDATAEXT WITH eDT_VENC
                              @ LIN,20 SAY 'para '+sisEXT+'.'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              SELE 8
                              @ LIN,00 SAY '|'
                              @ LIN,10 SAY 'Valor da Prestacao : '
                              @ LIN,30 SAY VLR_PARC   PICT '@R 999,999,999,999.99'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,10 SAY 'Multa/Juros/C.M.(+): '
                              eDIFERENCA = eMULTA + eCORRECAO + eJUROS
                              @ LIN,30 SAY eDIFERENCA PICT '@R 999,999,999,999.99'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,10 SAY 'Desconto (-)       : '
                              eDIFERENCA = eDESCONTO
                              @ LIN,30 SAY eDIFERENCA PICT '@R 999,999,999,999.99'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,10 SAY 'Valor Pagamento    : '
                              @ LIN,30 SAY VLR_PGTO   PICT '@R 999,999,999,999.99'
                              DO CASE
                                 CASE MOEDA = 'D'
                                    @ LIN,60 SAY 'PGTO : DINHEIRO'
                                 CASE MOEDA = 'C'
                                    @ LIN,60 SAY 'PGTO : CHEQUE'
                                 CASE MOEDA = 'B'
                                    @ LIN,60 SAY 'PGTO : BANCO'
                                 CASE MOEDA = 'O'
                                    @ LIN,60 SAY 'PGTO : OUTROS'
                                 CASE MOEDA = ' '
                                    @ LIN,60 SAY 'PGTO : INDEFINIDO'
                              ENDCASE
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,70  SAY '----------------------------------------------'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              eDT_PAGO = DTOC(eDATA_PAGO)
                              DO PDATAEXT WITH eDT_PAGO
                              @ LIN,10  SAY ALLTRIM(eCIDADE)+', '+sisEXT
                              @ LIN,70  SAY '      '+sisTIT1
                              @ LIN,70  SAY '      '+sisTIT1
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              @ LIN,76  SAY 'CGC  :' 
                              @ LIN,82  SAY eCPF_CGC PICT '@R 99.999.999/9999-99'
                              @ LIN,76  SAY 'CGC  :' 
                              @ LIN,82  SAY eCPF_CGC PICT '@R 99.999.999/9999-99'
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY '|'
                              IF eNUM_BANCO = SPACE(3)
                                 @ LIN,10  SAY 'Banco : ________________________ No. Cheque : ________________________ Observacao : ________________________'
                              ELSE
                                 @ LIN,10 SAY 'Banco/Ag/C.C/Cheque: '+eNUM_BANCO+'/'+ALLTRIM(eNUM_AGENC)+'/'+ALLTRIM(eNUM_CONTA)+'/'+ALLTRIM(eNUM_CHEQUE)
                              ENDIF
                              @ LIN,134 SAY '|'
                              LIN = LIN + 1
                              @ LIN,00  SAY REPLICATE('_',135)
                              LIN = LIN + 1
                              @ LIN,08  SAY REPLICATE('- ',60)
                              LIN = LIN + 2
                              @ LIN,08  SAY ' '
                              eVEZES = eVEZES + 1
                           ENDDO
                           SET DEVICE TO SCREEN
                           DO PLQUA WITH '01'
                           DO PPERG WITH 'Deseja Avancar a folha (S/N) ? '
                           IF sisRESP = 'S'
                              SET DEVICE TO PRINT
                              @ 00,00 SAY ' '
                              LIN  = 1
                              ePAG = 1
                              SET DEVICE TO SCREEN
                           ENDIF
                           SELE 8
                           IF F_BLOQR()
                              REPLACE COD_BANCO WITH STR(eNUM_RECI,10,0)
                              UNLOCK
                           ENDIF
                           SELE 9
                           eNUM_RECI = eNUM_RECI + 1
                           IF F_BLOQR()
                              REPLACE NUM_RECI WITH eNUM_RECI
                              UNLOCK
                           ENDIF
                           SELE 8
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
            SET CURSOR OFF
            RESTORE SCREEN FROM TELA04
         ELSE
           IF F_BLOQR()
              SAVE SCREEN TO TELA04
              SET CURSOR ON
              FLD_NAME = CAMPO[DB_COLUMN]
              FLD_PIC  = PIC[DB_COLUMN]
              @ ROW(),COL() GET &FLD_NAME PICTURE FLD_PIC
              READ
              SET CURSOR OFF
              RESTORE SCREEN FROM TELA04
              UNLOCK
           ENDIF
	 ENDIF
      CASE KEY == 27
	 RET = 0
   ENDCASE
ENDIF
RETURN RET
***************************** fim ***********************************
************************ funcao *********************************
FUNCTION CALC_PAG2
DO CASE
   CASE eDATA_VENC = eDATA_PAGO
      DO PLQUA WITH '01'
      DO PMENSAG WITH 'Pagamento dentro do Prazo, Aguarde ... 1'
      eDESCONTO = 0
      eMULTA    = 0
      eJUROS    = 0
      eCORRECAO = 0
   CASE eDATA_VENC > eDATA_PAGO
      * Calculo de Desconto de Parcelas
      DO PLQUA WITH '01'
      DO PMENSAG WITH 'Pagamento Antecipado, Aguarde ... 2'
      eDIA_VENC = 0
      eMES_VENC = 0
      eANO_VENC = 0
      eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
      eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
      eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
      eDIA_VENC = VAL(eDIA_VENC)
      eMES_VENC = VAL(eMES_VENC) - 1
      eANO_VENC = VAL(eANO_VENC) 
      IF eMES_VENC = 0
         eMES_VENC = 12
         eANO_VENC = eANO_VENC - 1
      ENDIF
      eDIA_VECC = eDIA_VENC
      eMES_VECC = eMES_VENC
      eANO_VECC = eANO_VENC
      eTAM_DIA  = LENNUM(INT(eDIA_VENC))
      eTAM_MES  = LENNUM(INT(eMES_VENC))
      eDIA_VENC = IIF(eTAM_DIA = 1,'0'+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
      eMES_VENC = IIF(eTAM_MES = 1,'0'+STR(eMES_VENC,1),STR(eMES_VENC,2))
      eANO_VENC = STR(INT(eANO_VENC),4)
      eMES_ANT  = CTOD(eDIA_VENC+'/'+eMES_VENC+'/'+eANO_VENC)
      DO CASE
         CASE eDATA_VENC > eDATA_PAGO .AND. eDATA_PAGO < eMES_ANT
         DO PLQUA WITH '01'
         DO PMENSAG WITH 'Calculo de Desconto, Aguarde ...'
         eMULTA    = 0
         eJUROS    = 0
         eCORRECAO = 0
         eAUX      = 1
         eDIA_VENC = 0
         eMES_VENC = 0
         eANO_VENC = 0
         eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
         eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
         eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
         eDIA_PAGO = 0
         eMES_PAGO = 0
         eANO_PAGO = 0
         eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
         eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
         eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
         eAUX_VENC = CTOD('')
         eDIA_VENC = VAL(eDIA_VENC)
         eMES_VENC = VAL(eMES_VENC)
         eANO_VENC = VAL(eANO_VENC)
         DO WHILE .T.
            eMES_VENC = eMES_VENC - 1
            IF eMES_VENC = 0
               eMES_VENC = 12
               eANO_VENC = eANO_VENC - 1
            ENDIF
            eAUX      = eAUX + 1
            eTAM_DIA  = LENNUM(INT(eDIA_VENC))
            eTAM_MES  = LENNUM(INT(eMES_VENC))
            eDIA_VENC = IIF(eDIA_VENC > 28 .AND. eMES_VENC = 2,28,eDIA_VENC)
            eDIA_VENC = IIF(eTAM_DIA = 1,'0'+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
            eMES_VENC = IIF(eTAM_MES = 1,'0'+STR(eMES_VENC,1),STR(eMES_VENC,2))
            eANO_VENC = STR(INT(eANO_VENC),4)
            eAUX_VENC = CTOD(eDIA_VENC+'/'+eMES_VENC+'/'+eANO_VENC)
            eDIA_VENC = VAL(IIF(eTAM_DIA = 1,'0'+ALLTRIM(eDIA_VENC),ALLTRIM(eDIA_VENC)))
            eMES_VENC = VAL(IIF(eTAM_MES = 1,'0'+ALLTRIM(eMES_VENC),ALLTRIM(eMES_VENC)))
            eANO_VENC = VAL(eANO_VENC)
            IF MONTH(eAUX_VENC) = MONTH(eDATA_PAGO) .AND. YEAR(eAUX_VENC) = YEAR(eDATA_PAGO)
               EXIT
            ENDIF
         ENDDO
         IF eAUX_VENC < eDATA_PAGO
            eAUX = eAUX - 1
         ENDIF
         * Calculo do desconto
         eDESCONTO = (eVLR_PARC * (eTX_DESCON * eAUX))
         eMULTA    = 0
         eJUROS    = 0
         eCORRECAO = 0
      OTHERWISE
         eDESCONTO = 0
         eMULTA    = 0
         eJUROS    = 0
         eCORRECAO = 0
      ENDCASE
   OTHERWISE
      * Calculo de Multa e Juros
      DO CASE
      CASE eDATA_PAGO > eDATA_VENC  .AND. ((eDATA_PAGO - eDATA_VENC) >= 30)
         DO PLQUA WITH '01'
         DO PMENSAG WITH 'Calculo de Multa e Juros, Aguarde ...3'
         eDESCONTO = 0
         eMULTA    = 0
         eJUROS    = 0
         eCORRECAO = 0
         eAUX      = 1
         eDIA_VENC = 0
         eMES_VENC = 0
         eANO_VENC = 0
         eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
         eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
         eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
         eDIA_VENO = eDIA_VENC
         eMES_VENO = eMES_VENC
         eANO_VENO = eANO_VENC
         eDIA_PAGO = 0
         eMES_PAGO = 0
         eANO_PAGO = 0
         eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
         eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
         eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
         eAUX_VENC = CTOD('')
         eDIA_VECC = VAL(eDIA_VENC)
         eMES_VECC = VAL(eMES_VENC)
         eANO_VECC = VAL(eANO_VENC)
         eMES_VECC = eMES_VECC + 1
         eDIF_DIAS = 30 - VAL(eDIA_PAGO)
         IF eMES_VECC = 13
            eMES_VECC = 01
            eANO_VECC = eANO_VECC + 1
         ENDIF
         eTAM_MES  = LENNUM(INT(eMES_VECC))
         eMES_VECC = IIF(eTAM_MES = 1,'0'+STR(eMES_VECC,1),STR(eMES_VECC,2))
         eANO_VECC = STR(INT(eANO_VECC),4)
         DO WHILE .T.
            eDIA_VENC = VAL(eDIA_VENC)
            eMES_VENC = VAL(eMES_VENC)
            eANO_VENC = VAL(eANO_VENC)
            eMES_VENC = eMES_VENC + 1
            IF eMES_VENC = 13
               eMES_VENC = 01
               eANO_VENC = eANO_VENC + 1
            ENDIF
            eAUX = eAUX + 1
            eTAM_DIA  = LENNUM(INT(eDIA_VENC))
            eTAM_MES  = LENNUM(INT(eMES_VENC))
            eDIA_VENC = IIF(eDIA_VENC > 28 .AND. eMES_VENC = 2,28,eDIA_VENC)
            eDIA_VENC = IIF(eTAM_DIA = 1,'0'+STR(eDIA_VENC,1),STR(eDIA_VENC,2))
            eMES_VENC = IIF(eTAM_MES = 1,'0'+STR(eMES_VENC,1),STR(eMES_VENC,2))
            eANO_VENC = STR(INT(eANO_VENC),4)
            eAUX_VENC = CTOD(eDIA_VENC+'/'+eMES_VENC+'/'+eANO_VENC)
            IF (MONTH(eAUX_VENC) = MONTH(eDATA_PAGO)) .AND. (YEAR(eAUX_VENC) = YEAR(eDATA_PAGO))
               EXIT
            ENDIF
         ENDDO
         IF eAUX_VENC > eDATA_PAGO
            eAUX = eAUX - 1
         ENDIF
         IF eDIF_DIAS > 0
            SELE 7
            GOTO TOP
            SET FILTER TO (ANO+MES = eANO_VENO+eMES_VENO)
            GOTO TOP
            eIND_ACUM = ((&eINDICE/100) + 1)
            eCORRECAO = eCORRECAO + (eVLR_PARC * ((eIND_ACUM ^ (( 1000 / 30 ) / 1000) ^ eDIF_DIAS) - 1))
         ENDIF
         SELE 7
         GOTO TOP
         SET FILTER TO ((ANO+MES >= eANO_VECC+eMES_VECC) .AND. (ANO+MES <= eANO_PAGO+eMES_PAGO))
         GOTO TOP
         eIND_ACUM  = 1
         DO WHILE (ANO+MES <= eANO_PAGO+eMES_PAGO) .AND. .NOT. EOF()
            eIND_ACUM = ((&eINDICE/100) + 1)
            eCORRECAO = eCORRECAO + (eVLR_PARC * (eIND_ACUM - 1))
            SKIP
         ENDDO
         IF ePERID_COMI = 'a.m.'
            eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eAUX - 1)) - 1)
         ELSE
            eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eDATA_PAGO - eDATA_VENC)) - 1)
         ENDIF
         * Calculo do Multa e Juros
         eMULTA    = (eVLR_PARC + eCORRECAO) * eTX_MULTA
         eDESCONTO = 0
      CASE eDATA_PAGO > eDATA_VENC .AND. ((eDATA_PAGO - eDATA_VENC) < 30)
         DO PLQUA WITH '01'
         DO PMENSAG WITH 'Calculo de Multa, Aguarde ...'
         eDESCONTO = 0
         eMULTA    = 0
         eJUROS    = 0
         eCORRECAO = 0
         eAUX      = 1
         eDIA_VENC = 0
         eMES_VENC = 0
         eANO_VENC = 0
         eDIA_VENC = SUBSTR(DTOC(eDATA_VENC),1,2)
         eMES_VENC = SUBSTR(DTOC(eDATA_VENC),4,2)
         eANO_VENC = SUBSTR(DTOC(eDATA_VENC),7,4)
         eDIA_VENO = eDIA_VENC
         eMES_VENO = eMES_VENC
         eANO_VENO = eANO_VENC
         eDIA_PAGO = 0
         eMES_PAGO = 0
         eANO_PAGO = 0
         eDIA_PAGO = SUBSTR(DTOC(eDATA_PAGO),1,2)
         eMES_PAGO = SUBSTR(DTOC(eDATA_PAGO),4,2)
         eANO_PAGO = SUBSTR(DTOC(eDATA_PAGO),7,4)
         eAUX_VENC = CTOD('')
         eDIA_VECC = VAL(eDIA_VENC)
         eMES_VECC = VAL(eMES_VENC)
         eANO_VECC = VAL(eANO_VENC)
         eMES_VECC = eMES_VECC + 1
         eDIF_DIAS = 30 - VAL(eDIA_PAGO)
         IF eMES_VECC = 13
            eMES_VECC = 01
            eANO_VECC = eANO_VECC + 1
         ENDIF
         eTAM_MES  = LENNUM(INT(eMES_VECC))
         eMES_VECC = IIF(eTAM_MES = 1,'0'+STR(eMES_VECC,1),STR(eMES_VECC,2))
         eANO_VECC = STR(INT(eANO_VECC),4)
         IF eAUX_VENC >= eDATA_PAGO
            eAUX = eAUX - 1
         ENDIF
         * Calculo de Correcao
         IF eDIF_DIAS > 0
            SELE 7
            GOTO TOP
            SET FILTER TO (ANO+MES = eANO_VENO+eMES_VENO)
            GOTO TOP
            eIND_ACUM = ((&eINDICE/100) + 1)
            eCORRECAO = eCORRECAO + (eVLR_PARC * ((eIND_ACUM ^ (( 1000 / 30 ) / 1000) ^ eDIF_DIAS) - 1))
         ENDIF
         eDESCONTO = 0
         IF ePERID_COMI = 'a.m.'
            eJUROS    = 0
         ELSE
            eJUROS    = (eVLR_PARC + eCORRECAO) * (((eTX_COMPER + 1) ^ (eDATA_PAGO - eDATA_VENC)) - 1)
         ENDIF
         eMULTA    = (eVLR_PARC + eCORRECAO) * eTX_MULTA
   ENDCASE
ENDCASE
RETURN 0
***************************** fim ***********************************
     

