*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : COR_IMOA.PRG
* Data        : 01/01/99
* Objetivo    : CORRECAO MONETARIA ANUAL DOS FINANCIAMENTOS
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION COR_IMOA()
SAVE SCREEN TO TELA03
PTIT( "CORRECAO MONETµRIA DE FINANCIAMENTO - ANUAL")
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
USE CAD_VEND INDEX CAD_VEND,DATA_VEN
GOTO TOP
SELE 7
USE TAXA_MES INDEX TAXA_MES
GOTO TOP
SELE 8
* Cadastro de Parcelas
SELE 9
USE INDICES INDEX INDICES
GOTO TOP
SELE 2
* Selecao do produto
DO WHILE .T.
   PLIMPA()
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
   eCLAS_PRO = CODIGO
   eTIPO_PRO = TIPO
   EXIT
ENDDO
* Selecao da rotina de Alteracao conforme o tipo de produto
eQUADRA = SPACE(1)
eQUADRA  = SPACE(1)
DO CASE
   CASE eTIPO_PRO = 1
      * Consulta de Loteamentos
      SELE 3
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA     = SPACE(5)
      eNUM_IMOVEL = SPACE(5)
      PLQUA("01")
      @ 06,02 SAY "Quadra  : "
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
      eNUM_IMOVEL = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar a Lote (S/N) ? ")
      IF sisRESP = "S"
         @ 07,02 SAY "Lote    : "
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
         GOTO TOP
         IF EOF()
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG("N∆o H† Imoveis Disponá√≠veis "!")
            PPARA()
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
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
         ePARTE      = STATUS
         IF ePARTE <> 0
            eOPCAO = 0
            PLQUA("01")
            @ 23,00 SAY "Informe Correá∆o Para o (1)o. ou (2)o. Meio Lote ? " GET eOPCAO PICT "99999" RANGE 1,2
            READ
            IF eOPCAO = 1
               ePARTE = 1
            ELSE
               ePARTE = 2
            ENDIF
         ENDIF
      ENDIF
CASE eTIPO_PRO = 2
      * Consulta de Apartamentos
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      eNUM_IMOVEL= SPACE(5)
      @ 06,02 SAY "Bloco   : "
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
      eQUANTIDADE = QUANTIDADE
      eNUM_IMOVEL = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar o Apartamento (S/N) ? ")
      IF sisRESP = "S"
         @ 07,02 SAY "Apto.   : "
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
         GOTO TOP
         IF EOF()
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG("N∆o H† Imoveis Disponá√≠veis "!")
            PPARA()
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
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
         ePARTE      = STATUS
      ENDIF
   CASE eTIPO_PRO = 3
      * Consulta de Salas Comerciais
      SELE 4
      SET FILTER TO NUM_LOTEAM = STR(eCLAS_PRO,5) .AND. CADASTRO
      GOTO TOP
      eQUADRA    = SPACE(5)
      @ 06,02 SAY "Bloco   : "
      GO TOP
      DECLARE CAMPO[3],PIC[3],CAB[3],LENS[3]
      AFIELDS(CAMPO,.T.,LENS)
      PIC[1]="@R99999"
      PIC[2]="@R99999"
      PIC[3]="@R99999"
      CAB[1]="    "
      CAB[2]="    "
      CAB[3]="    "
      DBEDIT(6,10,6,41,CAMPO,"FUNC_PROV",PIC,CAB," "," - "," ")
      eQUADRA     = NUM_BLOCO
      eQUANTIDADE = QUANTIDADE
      eNUM_IMOVEL = SPACE(5)
      PLQUA("01")
      PPERG ( "Deseja Selecionar o Apartamento (S/N) ? ")
      IF sisRESP = "S"
         @ 07,02 SAY "Apto.   : "
         SELE 1
         SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
         GOTO TOP
         IF EOF()
            PLQUA("01")
            PMSGR( "Atená∆o !",3)
            PMENSAG("N∆o H† Imoveis Disponá√≠veis "!")
            PPARA()
            CLOSE DATABASES
            RESTORE SCREEN FROM TELA03
            RETURN nil
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
         ePARTE      = STATUS
      ENDIF
   OTHERWISE
      * Consulta de Casa/Terrenos/etc...
      DO PDESENV
ENDCASE
eDATA_COR   = CTOD("")
DO WHILE .T.
   PLQUA("01")
   @ 23,00 SAY "Data Inicial da Correá∆o : " GET eDATA_COR
   READ
   IF eDATA_COR = CTOD("") 
      PLQUA("01")
      PMSGR( "Data Invalida !",3)
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
IF eNUM_IMOVEL = SPACE(5)
   SELE 6
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. ULT_CORREC < eDATA_COR
   GOTO TOP
ELSE
   SELE 6
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. ULT_CORREC < eDATA_COR
   GOTO TOP
ENDIF
eNUM_IMOVEL = NUM_IMOVEL
eQUADRA     = QUADRA
ePARTE      = PARTE      
eDT_VENDA   = DATA_VENDA
eINDICE     = INDICE
ePARCELAS   = PARCELAS
@ 07,00
@ 07,02 SAY "Lote    : " GET eNUM_IMOVEL 
eDATA_INI   = eDATA_COR
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 08,02 SAY "Correá∆o de : " GET eDATA_INI
   READ
   eDATA_FIM   = eDATA_INI + 365
   @ 09,02 SAY "Correá∆o atá: " GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR( "Datas Inv†lidas !",3)
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
PLQUA("01")
PPERG ( "Confirma a Correá∆o Monetária (S/N) ? ")
IF sisRESP = "N"
   CLOSE DATABASES
   RESTORE SCREEN FROM TELA03
   RETURN nil
ENDIF
eNUM_IMOVEL = NUM_IMOVEL
eQUADRA     = QUADRA
ePARTE      = PARTE      
eDT_VENDA   = DATA_VENDA
eINDICE     = INDICE
ePARCELAS   = PARCELAS
SELE 6 
GOTO TOP
DO WHILE .NOT. EOF()
   SELE 8
   eIND_CLAS = STR(eCLAS_PRO,5)
   eIND_QUAD = eQUADRA
   eZERO = "000"
   eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
   eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
   eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
   eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD)
   USE &eARQ INDEX &eIND1,&eIND2,&eIND3
   GOTO TOP
   * SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. VLR_PGTO = 0
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. ATIVA
   GOTO TOP
   IF EOF()
      PLQUA("01")
      PMSGR( "Atená∆o !",3)
      PMENSAG("N∆o H† parcelas á corrigir !")
      SELE 6
      SKIP
      eNUM_IMOVEL = NUM_IMOVEL
      eQUADRA     = QUADRA
      ePARTE      = PARTE      
      eDT_VENDA   = DATA_VENDA
      eINDICE     = INDICE
      ePARCELAS   = PARCELAS
      @ 07,00
      @ 07,02 SAY "No.Imovel: " GET eNUM_IMOVEL 
      CLEAR GETS
      LOOP
   ELSE
      SELE 8
      GOTO TOP
      eQUANT_PAR = 0
      DO WHILE .NOT. EOF()
         IF DATA_VENC > eDATA_FIM .AND. VLR_PGTO = 0
            eQUANT_PAR = eQUANT_PAR + 1
         ENDIF
         SKIP
      ENDDO
      SELE 8
      GOTO TOP
      eACUM_RES1 = 0
      eACUM_TAXA = 1
      DO WHILE .NOT. EOF()
         PLQUA("01")
         PMENSAG("Efetuando Correá∆o das Parcelas do IM‡VEL ("+ALLTRIM(STR(eCLAS_PRO,5))+"/"+ALLTRIM(eQUADRA)+"/"+ALLTRIM(eNUM_IMOVEL)+"/"+STR(ePARTE,1)+") Parcela No. "+NUM_PARC
         eMES_LIMIT = SUBSTR(DTOC(eDATA_FIM),4,2)
         eANO_LIMIT = SUBSTR(DTOC(eDATA_FIM),7,4)
         eVLR_ORIG  = VLR_PARC
         eDATA_VENC = DATA_VENC
         eMES_PARCE = SUBSTR(DTOC(eDATA_VENC),4,2)
         eANO_PARCE = SUBSTR(DTOC(eDATA_VENC),7,4)
         eMES_VENDA = SUBSTR(DTOC(eDATA_INI),4,2)
         eANO_VENDA = SUBSTR(DTOC(eDATA_INI),7,4)
         SELE 7         
         GOTO TOP
         SET FILTER TO ((ANO+MES >= eANO_VENDA+eMES_VENDA) .AND. (ANO+MES <= eANO_PARCE+eMES_PARCE)) .AND. (ANO+MES <= eANO_LIMIT+eMES_LIMIT)
         GOTO TOP
         eIND_ACUM  = 1
         DO WHILE (ANO+MES < eANO_PARCE+eMES_PARCE) .AND. .NOT. EOF()
            eIND_ACUM = ((&eINDICE/100) + 1) * eIND_ACUM
            SKIP
         ENDDO
         SELE 8
         IF DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM
            SELE 7         
            GOTO TOP
            SET FILTER TO ((ANO+MES >= eANO_PARCE+eMES_PARCE) .AND. (ANO+MES <= eANO_LIMIT+eMES_LIMIT)) 
            GOTO TOP
            eIND_RESI  = 1
            DO WHILE (ANO+MES < eANO_LIMIT+eMES_LIMIT) .AND. .NOT. EOF()
               eIND_RESI = ((&eINDICE/100) + 1) * eIND_RESI
               SKIP
            ENDDO
            SELE 8
            eACUM_RES1 = eACUM_RES1 + ((((VLR_ORIGIN * eIND_ACUM) - VLR_ORIGIN) / eQUANT_PAR) * eIND_RESI)
         ENDIF
         SELE 8
         IF DATA_VENC >= eDATA_FIM
            IF F_BLOQR()
               IF VLR_PGTO = 0
                  REPLACE VLR_PARC WITH (VLR_ORIGIN * eIND_ACUM) + eACUM_RES1
               ENDIF
               UNLOCK
            ENDIF
         ENDIF
         SELE 8
         SKIP
      ENDDO
   ENDIF
   SELE 6
   IF F_BLOQR()
      REPLACE ULT_CORREC WITH eDATA_FIM
      UNLOCK
   ENDIF
   SKIP
   eNUM_IMOVEL = NUM_IMOVEL
   eQUADRA     = QUADRA
   ePARTE      = PARTE      
   eDT_VENDA   = DATA_VENDA
   eINDICE     = INDICE
   ePARCELAS   = PARCELAS
   @ 07,00
   @ 07,02 SAY "No.Imovel: " GET eNUM_IMOVEL 
   CLEAR GETS
ENDDO
PLQUA("01")
PMSGR( "Processamento Concluádo !",3)
CLOSE DATABASES
RESTORE SCREEN FROM TELA03
RETURN nil
*************************  fim  *********************************
