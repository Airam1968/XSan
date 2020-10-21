*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : EXC_EPIS.PRG
* Data        : 01/01/08
* Objetivo    : Exportaá∆o de dados para receita EDF-PIS/COFINS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
SAVE SCREEN TO TELA05
DO PTIT WITH "EXPORTAÄ«O DE DADOS - EDF-PIS/COFINS"
DO PLQUA WITH "01"
DO PPERG WITH "Confirma (S/N) ? "
IF sisRESP = "N"
   RESTORE SCREEN FROM TELA05
   RETURN
ENDIF
SELE 1
USE UNI_IMOB INDEX UNI_IMOB
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
USE TIPO_PRO INDEX TIPO_PRO
GOTO TOP
SELE 7
USE CAD_VEND INDEX CAD_VEND,DATA_VEN
GOTO TOP
SELE 9
* area do arquivo a ser gerado
SELE 2
* Selecao do produto
DO WHILE .T.
   DO PLIMPA
   SET COLOR TO W/N,+W/R
   eCLAS_PRO = 0
   eTIPO_PRO = 0
   @ 05,02 SAY "Produto : "
   GO TOP
   DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
   AFIELDS(CAMPO,.T.,LENS)
   PIC[1]="@R99999"
   PIC[2]="@R99999"
   PIC[3]="@S40"
   CAB[1]="    "
   CAB[2]="    "
   CAB[3]="    "
   DBEDIT(5,15,5,70,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
   eCLAS_PRO  = CODIGO
   eTIPO_PRO  = TIPO
   eDESC_PRO  = DESCRICAO
   eCOD_CONTA = COD_CONTA
   eCOD_PIS   = COD_PIS
   EXIT
ENDDO
IF ALLTRIM(eCOD_PIS) = SPACE(4)
   DO PLQUA WITH "01"
   DO PMSGR WITH "Atená∆o !",3
   DO PMENSAG WITH "Tipo de Registro n∆o definido (F200/1800), Corrija!"
   DO PPARA
   CLOSE DATABASES
   RESTORE SCREEN FROM TELA03
   RETURN
ENDIF
eNOME_BLOCO = SPACE(10)
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO      
      GOTO TOP
      eQUADRA     = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
      eOPCAO = "A"
      DO PLQUA WITH "01"
      DO PPERG WITH "Deseja Selecionar "+sisGRUPO1+" (S/N) ? "
      IF sisRESP = "S"
         @ 06,02 SAY sisGRUPO1+"  : "
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
         DO PLQUA WITH "01"
         DO PPERG WITH "Deseja Selecionar "+sisUNID1+" (S/N) ? "
         IF sisRESP = "S"
            @ 07,02 SAY sisUNID1+"   : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               DO PLQUA WITH "01"
               DO PMSGR WITH "Atená∆o !",3
               DO PMENSAG WITH "N∆o H† Imoveis Dispon°veis !"
               DO PPARA
               CLOSE DATABASES
               RESTORE SCREEN FROM TELA03
               RETURN
            ENDIF
            DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
            AFIELDS(CAMPO,.T.,LENS)
            CAMPO[5]="PROPRIET_1"
            CAMPO[6]="PROPRIET_2"
            LENS[5] =5
            LENS[6] =5
            PIC[1]="@R99999"
            PIC[2]="@R99999"
            PIC[3]="@R99999"
            PIC[4]="@R99999"
            PIC[5]="@R99999"
            PIC[6]="@R99999"
            CAB[1]="     "
            CAB[2]="     "
            CAB[3]="     "
            CAB[4]="     "
            CAB[5]="     "
            CAB[6]="     "
            DBEDIT(7,15,7,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
            eNUM_IMOVEL = NUM_IMOVEL
            eSTATUS     = STATUS
            ePROPR_1    = PROPRIET_1
            ePROPR_2    = PROPRIET_2
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL 
            GOTO TOP
         ELSE
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA   
            GOTO TOP
         ENDIF
      ELSE
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5)  
         GOTO TOP
      ENDIF
   CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      eNUM_IMOVEL= SPACE(5)
      eOPCAO = "A"
      DO PLQUA WITH "01"
      DO PPERG WITH "Deseja Selecionar "+sisGRUPO2+" (S/N) ? "
      IF sisRESP = "S"
         @ 06,02 SAY sisGRUPO2+"  : "
         GO TOP
         DECLARE CAMPO[4],PIC[4],CAB[4],LENS[4]
         AFIELDS(CAMPO,.T.,LENS)
         PIC[1]="@R99999"
         PIC[2]="@R99999"
         PIC[3]="@S30"
         PIC[4]="@R99999"
         CAB[1]="    "
         CAB[2]="    "
         CAB[3]="    "
         CAB[4]="    "
         DBEDIT(6,08,6,76,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
         eQUADRA      = NUM_BLOCO
         eQUANTIDADE  = QUANTIDADE
         DO PLQUA WITH "01"
         DO PPERG WITH "Deseja Selecionar "+sisUNID2+" (S/N) ? "
         IF sisRESP = "S"
            @ 07,02 SAY sisUNID2+"  : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               DO PLQUA WITH "01"
               DO PMSGR WITH "Atená∆o !",3
               DO PMENSAG WITH "N∆o H† Imoveis Dispon°veis !"
               DO PPARA
               CLOSE DATABASES
               RESTORE SCREEN FROM TELA03
               RETURN
            ENDIF
            DECLARE CAMPO[6],PIC[6],CAB[6],LENS[6]
            AFIELDS(CAMPO,.T.,LENS)
            CAMPO[5]="PROPRIET_1"
            CAMPO[6]="PROPRIET_2"
            LENS[5] =5
            LENS[6] =5
            PIC[1]="@R99999"
            PIC[2]="@R99999"
            PIC[3]="@R99999"
            PIC[4]="@R99999"
            PIC[5]="@R99999"
            PIC[6]="@R99999"
            CAB[1]="     "
            CAB[2]="     "
            CAB[3]="     "
            CAB[4]="     "
            CAB[5]="     "
            CAB[6]="     "
            DBEDIT(7,15,7,69,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
            eNUM_IMOVEL = NUM_IMOVEL
            eSTATUS     = STATUS
            ePROPR_1    = PROPRIET_1
            ePROPR_2    = PROPRIET_2
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL 
            GOTO TOP
         ELSE
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA
            GOTO TOP
         ENDIF
      ELSE
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5)  
         GOTO TOP
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 08,02 SAY "Per°odo de : " GET eDATA_INI
   @ 08,27 SAY "Ate : "       GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      DO PLQUA WITH "01"
      DO PMSGR WITH "Datas Inv†lidas !",3
      LOOP
   ELSE
      IF eDATA_INI > eDATA_FIM
         DO PLQUA WITH "01"
         DO PMSGR WITH "Data Inicio > Que Final !",3
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDIF
ENDDO



eM200_09 = 0
eM200_12 = 0
eM200_13 = 0
*@ 15,00 SAY "Dados do Registro M200 para o Empreendimento : "
*@ 17,00 SAY "VL_TOT_CONT_CUM_PER (CAMPO 09): " GET eM200_09 PICT "@R 999,999,999,999.99"
*@ 18,00 SAY "VL_CONT_CUM_REC     (CAMPO 12): " GET eM200_12 PICT "@R 999,999,999,999.99"
*@ 19,00 SAY "VL_TOT_CONT_REC     (CAMPO 13): " GET eM200_13 PICT "@R 999,999,999,999.99"
*READ


DO WHILE .T.
   DO PLQUA WITH "01"
   eNOME_DIR = SPACE(20)
   eNOME_DIR = "C:\                 "
   DO PLQUA WITH "01"
   @ 23,00 SAY "Informe o Caminho para Gravaá∆o do Arquivo : " GET eNOME_DIR PICT "@S20"
   READ
   IF ALLTRIM(eNOME_DIR) = SPACE(20)
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
DO WHILE .T.
   DO PLQUA WITH "01"
   eNOME_ARQ = SPACE(8)
   eNOME_ARQ = "TESTE   " 
   DO PLQUA WITH "01"
   @ 23,00 SAY "Informe o Nome do Arquivo com 8 caracteres : " GET eNOME_ARQ PICT "@R XXXXXXXX"
   READ
   IF ALLTRIM(eNOME_ARQ) = SPACE(8)
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO

eCONTA_LINHAS = 0
eCONTA_LINHAS_BLOCO = 0

* Inicio da Geraá∆o dos arquivos
SET EXCLUSIVE ON
eARQ  = ALLTRIM(eNOME_DIR)+ALLTRIM(eNOME_ARQ)+".TXT"
* Preparaá∆o dos Dados para o arquivo PIS
DO PLQUA WITH "01"
DO PMENSAG WITH "Geraá∆o em ("+ALLTRIM(eARQ)+"), Tecle [ENTER]"
DO PPARA
SET PRINT ON
SET PRINTER TO &eARQ
eDIV = "|"
*********************************************************************
*** REGISTRO 0000 ***
SELE 9
USE PIS_0000
GOTO TOP
eCAMPO01 = ALLTRIM(CAMPO01)
eCAMPO02 = "003"
eCAMPO03 = ALLTRIM(CAMPO03)
eCAMPO04 = ALLTRIM(CAMPO04)
eCAMPO05 = ALLTRIM(CAMPO05)
eCAMPO06 = eDATA_INI
eCAMPO07 = eDATA_FIM
eCAMPO08 = ALLTRIM(CAMPO08)
eCAMPO09 = ALLTRIM(CAMPO09)
eCNPJ    = eCAMPO09
eCAMPO10 = ALLTRIM(CAMPO10)
eCAMPO11 = ALLTRIM(CAMPO11)
eCAMPO12 = ALLTRIM(CAMPO12)
eCAMPO13 = ALLTRIM(CAMPO13)
eCAMPO14 = ALLTRIM(CAMPO14)

eREG_0000 = " "
eREG_0000 = eDIV+eCAMPO01+eDIV+eCAMPO02+eDIV+eCAMPO03+eDIV+eCAMPO04+eDIV+eCAMPO05+eDIV
eREG_0000 = eREG_0000+SUBSTR(DTOC(eCAMPO06),1,2)+SUBSTR(DTOC(eCAMPO06),4,2)+SUBSTR(DTOC(eCAMPO06),7,4)+eDIV
eREG_0000 = eREG_0000+SUBSTR(DTOC(eCAMPO07),1,2)+SUBSTR(DTOC(eCAMPO07),4,2)+SUBSTR(DTOC(eCAMPO07),7,4)+eDIV
eREG_0000 = eREG_0000+eCAMPO08+eDIV+eCAMPO09+eDIV+eCAMPO10+eDIV+eCAMPO11+eDIV+eCAMPO12+eDIV+eCAMPO13+eDIV+eCAMPO14+eDIV

? eREG_0000
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1
*********************************************************************
*** REGISTRO 0001 ***
eREG_0001 = " "
eREG_0001 = eDIV+"0001"+eDIV+"0"+eDIV

? eREG_0001
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1
*********************************************************************
*** REGISTRO 0100 ***
SELE 9
USE PIS_0100
GOTO TOP
eCAMPO01 = ALLTRIM(CAMPO01)
eCAMPO02 = ALLTRIM(CAMPO02)
eCAMPO03 = ALLTRIM(CAMPO03)
eCAMPO04 = ALLTRIM(CAMPO04)
eCAMPO05 = ALLTRIM(CAMPO05)
eCAMPO06 = ALLTRIM(CAMPO06)
eCAMPO07 = ALLTRIM(CAMPO07)
eCAMPO08 = ALLTRIM(CAMPO08)
eCAMPO09 = ALLTRIM(CAMPO09)
eCAMPO10 = ALLTRIM(CAMPO10)
eCAMPO11 = ALLTRIM(CAMPO11)
eCAMPO12 = ALLTRIM(CAMPO12)
eCAMPO13 = ALLTRIM(CAMPO13)
eCAMPO14 = ALLTRIM(CAMPO14)

eREG_0100 = " "
eREG_0100 = eDIV+eCAMPO01+eDIV+eCAMPO02+eDIV+eCAMPO03+eDIV+eCAMPO04+eDIV+eCAMPO05+eDIV+eCAMPO06+eDIV+eCAMPO07+eDIV+eCAMPO08+eDIV
eREG_0100 = eREG_0100+eCAMPO09+eDIV+eCAMPO10+eDIV+eCAMPO11+eDIV+eCAMPO12+eDIV+eCAMPO13+eDIV+eCAMPO14+eDIV

? eREG_0100
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1
*********************************************************************
*** REGISTRO 0110 ***
SELE 9
USE PIS_0110
GOTO TOP
eCAMPO01 = ALLTRIM(CAMPO01)
eCAMPO02 = ALLTRIM(CAMPO02)
eCAMPO03 = ALLTRIM(CAMPO03)
eCAMPO04 = ALLTRIM(CAMPO04)

eREG_0110 = " "
*eREG_0110 = eDIV+eCAMPO01+eDIV+eCAMPO02+eDIV+eCAMPO03+eDIV+eCAMPO04+eDIV
*Vers∆o 2.0 ou maior
eREG_0110 = eDIV+eCAMPO01+eDIV+eCAMPO02+eDIV+eCAMPO03+eDIV+eCAMPO04+eDIV+"9"+eDIV

? eREG_0110
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1
*********************************************************************
*** REGISTRO 0140 ***
SELE 9
USE PIS_0140
GOTO TOP
eCAMPO01 = ALLTRIM(CAMPO01)
eCAMPO02 = ALLTRIM(CAMPO02)
eCAMPO03 = ALLTRIM(CAMPO03)
eCAMPO04 = ALLTRIM(CAMPO04)
eCAMPO05 = ALLTRIM(CAMPO05)
eCAMPO06 = ALLTRIM(CAMPO06)
eCAMPO07 = ALLTRIM(CAMPO07)
eCAMPO08 = ALLTRIM(CAMPO08)
eCAMPO09 = ALLTRIM(CAMPO09)


eREG_0140 = " "
eREG_0140 = eDIV+eCAMPO01+eDIV+eCAMPO02+eDIV+eCAMPO03+eDIV+eCAMPO04+eDIV+eCAMPO05+eDIV+eCAMPO06+eDIV+eCAMPO07+eDIV+eCAMPO08+eDIV+eCAMPO09+eDIV

? eREG_0140
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1
*********************************************************************
*** REGISTRO 0990 ***
eREG_0990 = " "
eREG_0990 = eDIV+"0990"+eDIV+ALLTRIM(STR(eCONTA_LINHAS_BLOCO+1))+eDIV

? eREG_0990
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = 0
*********************************************************************
*** REGISTRO A ***
eREG_A001 = "A001"
eREG_A001 = eDIV+eREG_A001+eDIV+"1"+eDIV
? eREG_A001
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

eREG_A990 = "A990"
eREG_A990 = eDIV+eREG_A990+eDIV+"2"+eDIV
? eREG_A990
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*********************************************************************
*** REGISTRO C ***
eREG_C001 = "C001"
eREG_C001 = eDIV+eREG_C001+eDIV+"1"+eDIV
? eREG_C001
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

eREG_C990 = "C990"
eREG_C990 = eDIV+eREG_C990+eDIV+"2"+eDIV
? eREG_C990
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*********************************************************************
*** REGISTRO D ***
eREG_D001 = "D001"
eREG_D001 = eDIV+eREG_D001+eDIV+"1"+eDIV
? eREG_D001
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

eREG_D990 = "D990"
eREG_D990 = eDIV+eREG_D990+eDIV+"2"+eDIV
? eREG_D990
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*********************************************************************
*** REGISTRO F001 ***
eREG_F001 = " "
eREG_F001 = eDIV+"F001"+eDIV+"0"+eDIV

? eREG_F001
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = 0
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1
*********************************************************************
*** REGISTRO F010 ***
eREG_F010 = " "
eREG_F010 = eDIV+"F010"+eDIV+eCNPJ+eDIV

? eREG_F010
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1
*********************************************************************
*** REGISTRO F200 ***
IF eCOD_PIS = "F200"
	SELE 9
	USE PIS_F200
	GOTO TOP
	eCAMPO01 = "F200"             
	eCAMPO02 = "04"               
	eCAMPO03 = "04"               
	eCAMPO04 = ALLTRIM(eDESC_PRO) 
	
	eCAMPO05 = ALLTRIM(CAMPO05)
	eCAMPO06 = ALLTRIM(CAMPO06)
	eCAMPO07 = ALLTRIM(CAMPO07)
	eCAMPO08 = CAMPO08
	eCAMPO09 = CAMPO09
	eCAMPO10 = CAMPO10
	eCAMPO11 = CAMPO11
	eCAMPO12 = "01"                
	eCAMPO13 = CAMPO13
	eCAMPO14 = "0.65"              
	eCAMPO15 = CAMPO15
	eCAMPO16 = "01"                
	eCAMPO17 = CAMPO17
	eCAMPO18 = "3.00"              
	eCAMPO19 = CAMPO19
	eCAMPO20 = CAMPO20
	eCAMPO21 = "4"                 
	eCAMPO22 = ALLTRIM(CAMPO22)
	
	* Dados da Venda e Clientes
	SELE 1
	GOTO TOP
	DO WHILE .NOT. EOF()
	   SELE 1
	   eSTATUS     = STATUS
	   eQUADRA     = QUADRA
	   eNUM_IMOVEL = NUM_IMOVEL
	   IF eSTATUS = 0
	      ePARTE = 0
	   ELSE
	      ePARTE = 1                                                    
	   ENDIF
	   SELE 7
	   GOTO TOP
	   SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
	   eDATA_VENDA    = DATA_VENDA
	*   IF eDATA_VENDA >= eDATA_INI .AND. eDATA_VENDA <= eDATA_FIM .AND. eSTATUS = 0
	      IF eSTATUS = 0
		 SELE 1
		 ePROPRIET_1 = PROPRIET_1
		 SELE 5
		 SEEK ePROPRIET_1
		 eCPF_CGC = CPF_CGC
		 * CGC ou CPF Sacado
		 eTAM_CGC_CPF = LEN(eCPF_CGC)
		 eAUX_CGC     = SPACE(15)
		 eAUX = 1
		 DO WHILE eAUX <= eTAM_CGC_CPF
		    IF SUBSTR(eCPF_CGC,eAUX,1) = "." .OR. SUBSTR(eCPF_CGC,eAUX,1) = "-" .OR. SUBSTR(eCPF_CGC,eAUX,1) = "/" .OR. SUBSTR(eCPF_CGC,eAUX,1) = " "
		       eAUX = eAUX + 1
		    ELSE
		       eAUX_CGC = ALLTRIM(eAUX_CGC) + SUBSTR(eCPF_CGC,eAUX,1)
		       eAUX = eAUX + 1
		    ENDIF
		 ENDDO
		 eCPF_CON = CPF_CONJ
		 * CGC ou CPF Conjuge
		 eTAM_CGC_CPF = LEN(eCPF_CON)
		 eAUX_CON     = SPACE(15)
		 eAUX = 1
		 DO WHILE eAUX <= eTAM_CGC_CPF
		    IF SUBSTR(eCPF_CON,eAUX,1) = "." .OR. SUBSTR(eCPF_CON,eAUX,1) = "-" .OR. SUBSTR(eCPF_CON,eAUX,1) = "/" .OR. SUBSTR(eCPF_CON,eAUX,1) = " "
		       eAUX = eAUX + 1
		    ELSE
		       eAUX_CON = ALLTRIM(eAUX_CON) + SUBSTR(eCPF_CON,eAUX,1)
		       eAUX = eAUX + 1
		    ENDIF
		 ENDDO
	
		 SELE 4
		 SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. NUM_BLOCO = eQUADRA .AND. CADASTRO
		 GOTO TOP
	
		 eCAMPO05 = ALLTRIM(eNUM_IMOVEL)+"/"+ALLTRIM(DESCRICAO)
		 ***
		 eCAMPO06 = ALLTRIM(STR(eCLAS_PRO,5))+ALLTRIM(eQUADRA)+ALLTRIM(eNUM_IMOVEL)+ALLTRIM(STR(ePARTE,1))
		 ***
		 eCAMPO07 = ALLTRIM(eAUX_CGC)
		 ***
		 SELE 7
		 eCAMPO08 = SUBSTR(DTOC(DATA_VENDA),1,2)+SUBSTR(DTOC(DATA_VENDA),4,2)+SUBSTR(DTOC(DATA_VENDA),7,4)
		 ***
		 eCAMPO22 = ALLTRIM(eAUX_CON)
		 SELE 1


		 *********************************************************************
		 * Dados das Parcelas dos Clientes
		 eTOT_MES = 0
		 * Abrir arquivo de parcelas
		 SELE 8
		 eIND_CLAS = STR(eCLAS_PRO,5)
		 eIND_QUAD = eQUADRA
		 eZERO = "000"
		 eARQP = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
		 eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
		 eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
		 eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
		 SELE 8
		 USE &eARQP INDEX &eIND1,&eIND2,&eIND3
		 GOTO TOP

		 * Total atualizado das parcelas
		 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. ATIVA
                 GOTO TOP
                 eTOT_MES	 = 0	 
		 eTOT_FINANC     = 0
		 eTOT_FINANC_ANT = 0
		 eTOT_FINANC_MES = 0
		 eTOT_PAGTO_ACUM = 0
		 eTOT_PAGTO_MES  = 0		 
		 
		 DO WHILE .NOT. EOF()
  		    eTOT_FINANC  = eTOT_FINANC + VLR_PARC
  		    * TOTAL PAGO ATE O MES ANTERIOR
                    IF (DATA_VENC < eDATA_INI) .AND. (VLR_PGTO>0)
                       eTOT_FINANC_ANT  = eTOT_FINANC_ANT + VLR_PARC                    
                    ENDIF
                    * TOTAL PAGO NO MES DA ESCRITURACAO
                    IF (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. (VLR_PGTO>0)
                       eTOT_FINANC_MES  = eTOT_FINANC_MES + VLR_PARC                    
                    ENDIF                    
		    SELE 8
		    SKIP
		 ENDDO
		 
		 eCAMPO09    = eTOT_FINANC
		 eCAMPO10    = eTOT_FINANC_ANT
		 eCAMPO11    = eTOT_FINANC_MES		 

		 eTOT_MES    = eTOT_FINANC_MES
	
		 eCAMPO13    = eTOT_FINANC_MES
		 eCAMPO15    = eTOT_FINANC_MES 
		 eCAMPO17    = eTOT_FINANC_MES		 
		 eCAMPO19    = eTOT_FINANC_MES
		 
		 eTOT_PAGTO_MES = eTOT_FINANC_MES

	
		 eTOT_PAGTO_MES  = INT(ROUND((eTOT_PAGTO_MES*100),0))
		 eTAM_PARC       = LENNUM(eTOT_PAGTO_MES)
		 eTOT_PAGTO_MES  = STR(eTOT_PAGTO_MES,eTAM_PARC-2,2)
		 *eTOT_PAGTO_MES  = ALLTRIM(eTOT_PAGTO_MES)
	
	
		 ***
		 IF eCOD_PIS = "F200"
 
		    eCAMPO15  = eCAMPO15 * (VAL(eCAMPO14)/100)
		    eTAM_PARC = LENNUM(eCAMPO15*1000)		    
		    eCAMPO15  = STR(eCAMPO15*1000,eTAM_PARC,0)
		    IF eTAM_PARC <= 0
		       eCAMPO15 = "0,00"
		    ELSE
		      IF eTAM_PARC = 1
		         eCAMPO15 = "0,0"+ALLTRIM(eCAMPO15)
		      ELSE
		        IF eTAM_PARC = 2
		           eCAMPO15 = "0,"+ALLTRIM(eCAMPO15)
		        ELSE
                           eCAMPO15 = ALLTRIM(SUBSTR(eCAMPO15,1,eTAM_PARC-3)+","+SUBSTR(eCAMPO15,eTAM_PARC-2,2))
                        ENDIF   
                      ENDIF
                    ENDIF
                    
		    eCAMPO19  = eCAMPO19 * (VAL(eCAMPO18)/100)
		    eTAM_PARC = LENNUM(eCAMPO19*1000)
		    eCAMPO19  = STR(eCAMPO19*1000,eTAM_PARC,0)
		    IF eTAM_PARC <= 0
		       eCAMPO19 = "0,00"
		    ELSE
		      IF eTAM_PARC = 1
		         eCAMPO19 = "0,0"+ALLTRIM(eCAMPO19)
		      ELSE
		        IF eTAM_PARC = 2
		           eCAMPO19 = "0,"+ALLTRIM(eCAMPO19)
		        ELSE
                           eCAMPO19 = ALLTRIM(SUBSTR(eCAMPO19,1,eTAM_PARC-3)+","+SUBSTR(eCAMPO19,eTAM_PARC-2,2))
                        ENDIF   
                      ENDIF
                    ENDIF


		    
		 ELSE
	
		    eCAMPO15  = eCAMPO15 * (VAL(eCAMPO14)/100)
		    eTAM_PARC = LENNUM(eCAMPO15*1000)		    
		    eCAMPO15  = STR(eCAMPO15*1000,eTAM_PARC,0)
		    IF eTAM_PARC <= 0
		       eCAMPO15 = "0,00"
		    ELSE
		      IF eTAM_PARC = 1
		         eCAMPO15 = "0,0"+ALLTRIM(eCAMPO15)
		      ELSE
		        IF eTAM_PARC = 2
		           eCAMPO15 = "0,"+ALLTRIM(eCAMPO15)
		        ELSE
                           eCAMPO15 = ALLTRIM(SUBSTR(eCAMPO15,1,eTAM_PARC-3)+","+SUBSTR(eCAMPO15,eTAM_PARC-2,2))
                        ENDIF   
                      ENDIF
                    ENDIF


		    
		    eCAMPO19  = eCAMPO19 * (VAL(eCAMPO18)/100)
		    eTAM_PARC = LENNUM(eCAMPO19*1000)
		    eCAMPO19  = STR(eCAMPO19*1000,eTAM_PARC,0)
		    IF eTAM_PARC <= 0
		       eCAMPO19 = "0,00"
		    ELSE
		      IF eTAM_PARC = 1
		         eCAMPO19 = "0,0"+ALLTRIM(eCAMPO19)
		      ELSE
		        IF eTAM_PARC = 2
		           eCAMPO19 = "0,"+ALLTRIM(eCAMPO19)
		        ELSE
                           eCAMPO19 = ALLTRIM(SUBSTR(eCAMPO19,1,eTAM_PARC-3)+","+SUBSTR(eCAMPO19,eTAM_PARC-2,2))
                        ENDIF   
                      ENDIF
                    ENDIF

	
		 ENDIF
	
	
		 *********************************************************************
	
	
	
		 eREG_F200 = " "
		 eREG_F200 = eDIV+eCAMPO01+eDIV+eCAMPO02+eDIV+eCAMPO03+eDIV+eCAMPO04+eDIV+eCAMPO05+eDIV+eCAMPO06+eDIV+eCAMPO07+eDIV+eCAMPO08+eDIV
	
		 eVLR_CAMPO = eCAMPO09
		 eTAM_CAMPO = LENNUM(eVLR_CAMPO*1000)
		 eVLR_CAMPO = STR(eVLR_CAMPO*1000,eTAM_CAMPO,0)
                 eVLR_CAMPO = ALLTRIM(SUBSTR(eVLR_CAMPO,1,eTAM_CAMPO-3)+","+SUBSTR(eVLR_CAMPO,eTAM_CAMPO-2,2))
	
		 eREG_F200 = eREG_F200+eVLR_CAMPO+eDIV

		 eVLR_CAMPO = eCAMPO10
		 IF eVLR_CAMPO <> 0
		    eTAM_CAMPO = LENNUM(eVLR_CAMPO*1000)
		    eVLR_CAMPO = STR(eVLR_CAMPO*1000,eTAM_CAMPO,0)
                    eVLR_CAMPO = ALLTRIM(SUBSTR(eVLR_CAMPO,1,eTAM_CAMPO-3)+","+SUBSTR(eVLR_CAMPO,eTAM_CAMPO-2,2))		 
		 ELSE
		    eVLR_CAMPO = "0,00"
		 ENDIF
		 
		 eREG_F200 = eREG_F200+eVLR_CAMPO+eDIV
	
		 eVLR_CAMPO = eCAMPO11
		 eTAM_CAMPO = LENNUM(eVLR_CAMPO*1000)
		 eVLR_CAMPO = STR(eVLR_CAMPO*1000,eTAM_CAMPO,0)
                 eVLR_CAMPO = ALLTRIM(SUBSTR(eVLR_CAMPO,1,eTAM_CAMPO-3)+","+SUBSTR(eVLR_CAMPO,eTAM_CAMPO-2,2))
	
		 eREG_F200 = eREG_F200+eVLR_CAMPO+eDIV


		 eREG_F200 = eREG_F200+eCAMPO12+eDIV
	
		 eVLR_CAMPO = eCAMPO13
		 eTAM_CAMPO = LENNUM(eVLR_CAMPO*1000)
		 eVLR_CAMPO = STR(eVLR_CAMPO*1000,eTAM_CAMPO,0)
                 eVLR_CAMPO = ALLTRIM(SUBSTR(eVLR_CAMPO,1,eTAM_CAMPO-3)+","+SUBSTR(eVLR_CAMPO,eTAM_CAMPO-2,2))
	
		 eREG_F200 = eREG_F200+eVLR_CAMPO+eDIV

		 eVLR_CAMPO = VAL(eCAMPO14)
		 eTAM_CAMPO = LENNUM(eVLR_CAMPO*1000)
		 eVLR_CAMPO = STR(eVLR_CAMPO*1000,eTAM_CAMPO,0)
                 eVLR_CAMPO = ALLTRIM(SUBSTR(eVLR_CAMPO,1,eTAM_CAMPO-3)+","+SUBSTR(eVLR_CAMPO,eTAM_CAMPO-2,2))
                 eVLR_CAMPO = "0,65"
		 
		 eREG_F200 = eREG_F200+eVLR_CAMPO+eDIV
	
		 *** Valor PIS eCAMPO15
	
		 eREG_F200 = eREG_F200+eCAMPO15+eDIV
	
		 eREG_F200 = eREG_F200+eCAMPO16+eDIV
	
		 eVLR_CAMPO = eCAMPO17
		 eTAM_CAMPO = LENNUM(eVLR_CAMPO*1000)
		 eVLR_CAMPO = STR(eVLR_CAMPO*1000,eTAM_CAMPO,0)
                 eVLR_CAMPO = ALLTRIM(SUBSTR(eVLR_CAMPO,1,eTAM_CAMPO-3)+","+SUBSTR(eVLR_CAMPO,eTAM_CAMPO-2,2))
	
		 eREG_F200 = eREG_F200+eVLR_CAMPO+eDIV

		 eVLR_CAMPO = VAL(eCAMPO18)
		 eTAM_CAMPO = LENNUM(eVLR_CAMPO*1000)
		 eVLR_CAMPO = STR(eVLR_CAMPO*1000,eTAM_CAMPO,0)
                 eVLR_CAMPO = ALLTRIM(SUBSTR(eVLR_CAMPO,1,eTAM_CAMPO-3)+","+SUBSTR(eVLR_CAMPO,eTAM_CAMPO-2,2))		 
                 eVLR_CAMPO = "3,00"
		 
		 eREG_F200 = eREG_F200+eVLR_CAMPO+eDIV
		  
		 eREG_F200 = eREG_F200+eCAMPO19+eDIV

		 * PERC_REC_RECEB

		 eVLR_CAMPO = 0		 
		 eVLR_CAMPO = (eCAMPO10+eCAMPO11)		 
		 IF (eVLR_CAMPO<=0) .OR. (eCAMPO09<=0) 
		     eVLR_CAMPO = "0,00"
		 ELSE    
			 eVLR_CAMPO = 0
			 eVLR_CAMPO = ((eCAMPO10+eCAMPO11)/eCAMPO09)	
			 
			 *? "Campo09 "
			 *?? eCampO09		 
			 *? "Campo010 "
			 *?? eCampO10		 
			 *? "Campo011 "
			 *?? eCampO11
			 IF eVLR_CAMPO <= 0
			 	eVLR_CAMPO = "0,00"			 
			 ELSE
				 eTAM_CAMPO = LEN(ALLTRIM(STR(eVLR_CAMPO*1000)))
				 *? "eTAM_CAMPO "
				 *?? eTAM_CAMPO		 
				 
				 eVLR_CAMPO = STR(eVLR_CAMPO*1000,eTAM_CAMPO,0)
				 
				 *? "eVLR_CAMPO "
				 *?? eVLR_CAMPO
				 
				 *DO PPARA
				 
				 *eTAM_CAMPO = eTAM_CAMPO - 4		 
				 IF eTAM_CAMPO = 0 .OR. VAL(eVLR_CAMPO) <= 0
				    eVLR_CAMPO = "0,00"
				 ELSE
				    IF eTAM_CAMPO = 1
				       eVLR_CAMPO = "0,0"+SUBSTR(ALLTRIM(eVLR_CAMPO),1,1)
				    ELSE
				      IF eTAM_CAMPO = 2
					 eVLR_CAMPO = "0,"+SUBSTR(ALLTRIM(eVLR_CAMPO),1,2)
				      ELSE
					 eVLR_CAMPO = ALLTRIM(SUBSTR(eVLR_CAMPO,1,eTAM_CAMPO-3)+","+SUBSTR(eVLR_CAMPO,eTAM_CAMPO-2,2))
				      ENDIF   
				   ENDIF
				 ENDIF
				 *? "eVLR_CAMPO "
				 *?? eVLR_CAMPO
				 
				 *DO PPARA
			 ENDIF
                 ENDIF   
                    
		 eREG_F200 = eREG_F200+eVLR_CAMPO+eDIV
	
	
		 eREG_F200 = eREG_F200+eCAMPO21+eDIV+eCAMPO22+eDIV
	
		 *** Escriturar apenas unidades com recebimentos no mes
		 IF eTOT_MES <> 0
		   ? eREG_F200
		   eCONTA_LINHAS = eCONTA_LINHAS + 1
		   eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1		   
		 ENDIF
		 *********************************************************************
	
	      ENDIF
	*   ENDIF
	   SELE 1
	   SKIP
	ENDDO
endif

*********************************************************************
*********************************************************************
*** REGISTRO F990 ***
eREG_F990 = " "
eREG_F990 = eDIV+"F990"+eDIV+ALLTRIM(STR(eCONTA_LINHAS_BLOCO+1))+eDIV

? eREG_F990
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = 0
*********************************************************************
*** REGISTRO 1800 ***
IF eCOD_PIS = "1800"
	*********************************************************************
	*** REGISTRO 1001 ***
	eREG_1001 = " "
	eREG_1001 = eDIV+"1001"+eDIV+"0"+eDIV
	
	? eREG_1001
	eCONTA_LINHAS = eCONTA_LINHAS + 1
	eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1
	*********************************************************************
	SELE 9
	USE PIS_1800
	GOTO TOP
	eCAMPO01 = "1800"             
	eCAMPO02 = ALLTRIM(eDESC_PRO) 
	eCAMPO03 = ALLTRIM(CAMPO03)
	eCAMPO04 = ALLTRIM(CAMPO04)
	eCAMPO05 = ALLTRIM(CAMPO05)
	eCAMPO06 = ALLTRIM(CAMPO06)
	eCAMPO07 = ALLTRIM(CAMPO07)
	eCAMPO08 = ALLTRIM(SUBSTR(DTOC(CAMPO08),1,2)+SUBSTR(DTOC(CAMPO08),4,2)+SUBSTR(DTOC(CAMPO08),7,4))
	eCAMPO09 = ALLTRIM(CAMPO09)
	
	* Dados da Venda e Clientes
	SELE 1
	GOTO TOP
	DO WHILE .NOT. EOF()
	   SELE 1
	   eSTATUS     = STATUS
	   eQUADRA     = QUADRA
	   eNUM_IMOVEL = NUM_IMOVEL
	   IF eSTATUS = 0
	      ePARTE = 0
	   ELSE
	      ePARTE = 1                                                    
	   ENDIF
	   SELE 7
	   GOTO TOP
	   SEEK STR(eCLAS_PRO,5)+eQUADRA+eNUM_IMOVEL+STR(ePARTE,1)
	   eDATA_VENDA    = DATA_VENDA
	*   IF eDATA_VENDA >= eDATA_INI .AND. eDATA_VENDA <= eDATA_FIM .AND. eSTATUS = 0
	      IF eSTATUS = 0
		 *********************************************************************
		 * Dados das Parcelas dos Clientes
	
		 * Abrir arquivo de parcelas
		 SELE 8
		 eIND_CLAS = STR(eCLAS_PRO,5)
		 eIND_QUAD = eQUADRA
		 eZERO = "000"
		 eARQP = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
		 eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
		 eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
		 eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
		 SELE 8
		 USE &eARQP INDEX &eIND1,&eIND2,&eIND3
		 GOTO TOP
	
		 * Filtro das parcelas pagas do periodo
		 eTOT_ACUM = 0
		 eTOT_JURO = 0
		 eTOT_ACU2 = " "
		 eTOT_JUR2 = " "         
		 SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM) .AND. ATIVA
		 GOTO TOP
		 DO WHILE .NOT. EOF()
		    
		    eTOT_ACUM  = eTOT_ACUM + VLR_PGTO
	
		    IF (VLR_PGTO > 0) .AND. (VLR_PGTO > VLR_PARC)
	
		       eTOT_JURO  = eTOT_JURO + (VLR_PGTO - VLR_PARC)
	
		    ENDIF
	
		    SELE 8
		    SKIP
		 ENDDO
		 ***
	
		 eTOT_ACUM  = INT(ROUND((eTOT_ACUM*100),0))
		 eTAM_PARC  = LENNUM(eTOT_ACUM)
		 eTOT_ACU2  = STR(eTOT_ACUM,eTAM_PARC,0)
		 eTOT_ACU2  = ALLTRIM(eTOT_ACU2)
	
		 eCAMPO03 = ALLTRIM(eTOT_ACU2)
		 
		 ***
	
		 eTOT_JURO  = INT(ROUND((eTOT_JURO*100),0))
		 eTAM_PARC  = LENNUM(eTOT_JURO)
		 IF eTOT_JURO > 0
                   eTOT_JUR2  = STR(eTOT_JURO,eTAM_PARC,0)
		   eTOT_JUR2  = ALLTRIM(eTOT_JUR2)
	         ELSE
		   eTOT_JUR2  = ""	         
	         ENDIF
	         
		 eCAMPO04 = ALLTRIM(eTOT_JUR2)
	
		 ***
		 
		 eTOT_ACUM  = INT(ROUND(((eTOT_ACUM+eTOT_JURO)*100),0))
		 eTAM_PARC  = LENNUM(eTOT_ACUM)
		 eTOT_ACU2  = STR(eTOT_ACUM,eTAM_PARC,0)
		 eTOT_ACU2  = ALLTRIM(eTOT_ACU2)		 
		 
		 eCAMPO05 = ALLTRIM(eTOT_ACU2)	
		 
		 ***

		 eCAMPO06 = "1,00"

		 ***

		 
		 
		 *********************************************************************

	
		 eREG_1800 = ""
		 eREG_1800 = eDIV+eCAMPO01+eDIV+eCAMPO02+eDIV+eCAMPO03+eDIV+eCAMPO04+eDIV+eCAMPO05+eDIV+eCAMPO06+eDIV+eCAMPO07+eDIV+eCAMPO08+eDIV
	
	
		 eREG_1800 = eREG_1800+eCAMPO09+eDIV
	
	         if (eTOT_ACUM<>0) .or. (eTOT_JURO<>0)
		   ? eREG_1800
		   eCONTA_LINHAS = eCONTA_LINHAS + 1
		   eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1		   
		 endif  
		 *********************************************************************
	
	      ENDIF
	*   ENDIF
	   SELE 1
	   SKIP
	ENDDO
	*********************************************************************
	*** REGISTRO 1990 ***
	eREG_1990 = " "
	eREG_1990 = eDIV+"1990"+eDIV+STR(eCONTA_LINHAS_BLOCO+1)+eDIV
	
	? eREG_1990
	eCONTA_LINHAS = eCONTA_LINHAS + 1
	eCONTA_LINHAS_BLOCO = 0
	*********************************************************************	

ENDIF
*********************************************************************

*** REGISTRO M ***
eREG_M001 = "M001"
eREG_M001 = eDIV+eREG_M001+eDIV+"1"+eDIV
? eREG_M001
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

eREG_M990 = "M990"
eREG_M990 = eDIV+eREG_M990+eDIV+"2"+eDIV
? eREG_M990
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*********************************************************************

*** REGISTRO 1001 ***
eREG_1001 = "1001"
eREG_1001 = eDIV+eREG_1001+eDIV+"1"+eDIV
? eREG_1001
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 1990 ***
eREG_1990 = "1990"
eREG_1990 = eDIV+eREG_1990+eDIV+"2"+eDIV
? eREG_1990
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1


*********************************************************************

*** REGISTRO 9001 ***
eREG_9001 = "9001"
eREG_9001 = eDIV+eREG_9001+eDIV+"0"+eDIV
? eREG_9001
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*********************************************************************

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eCONTA_LINHAS_BLOCO = 1
eREG_9900 = eDIV+eREG_9900+eDIV+"0000"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"0001"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"0100"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1


*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"0110"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"0140"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"0150"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"0150"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"0190"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"0200"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"0990"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"A001"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"A010"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"A100"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"A170"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"A990"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"C001"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"C990"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"D001"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"D990"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"F001"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"F010"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"F200"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"F990"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"M001"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"M990"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"1001"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"1990"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"9001"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"9900"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"9990"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*** REGISTRO 9900 ***
eREG_9900 = "9900"
eREG_9900 = eDIV+eREG_9900+eDIV+"9999"+eDIV+"1"+eDIV
? eREG_9900
eCONTA_LINHAS = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1


*********************************************************************

*** REGISTRO 9990 ***
eREG_9990 = "9990"
eCONTA_LINHAS  = eCONTA_LINHAS + 1
eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 2
eTAM_LINHAS    = LENNUM(eCONTA_LINHAS_BLOCO)
eCONTA_LINHAS_BLOCO  = STR(eCONTA_LINHAS_BLOCO,eTAM_LINHAS,0)
eREG_9990 = eDIV+eREG_9990+eDIV+eCONTA_LINHAS_BLOCO+eDIV
? eREG_9990
eCONTA_LINHAS = eCONTA_LINHAS + 1
*eCONTA_LINHAS_BLOCO = eCONTA_LINHAS_BLOCO + 1

*********************************************************************

*** REGISTRO 9999 ***
eREG_9999 = "9999"
*eCONTA_LINHAS  = eCONTA_LINHAS + 2
eTAM_LINHAS    = LENNUM(eCONTA_LINHAS)
eCONTA_LINHAS  = STR(eCONTA_LINHAS,eTAM_LINHAS,0)
eREG_9999 = eDIV+eREG_9999+eDIV+eCONTA_LINHAS+eDIV
? eREG_9999


*********************************************************************



SET PRINT OFF

SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA05
RETURN
*************************  fim  *********************************
