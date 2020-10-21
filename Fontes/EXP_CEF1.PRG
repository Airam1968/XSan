*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : EXC_CEF1.PRG
* Data        : 01/01/08
* Objetivo    : Exportaá∆o de dados para cef Terra Simao
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
*********************************************************************
FUNCTION EXP_CEF1
SAVE SCREEN TO TELA05
PTIT("EXPORTAÄ«O DE DADOS - CEF")
PLQUA("01")
PPERG("Confirma (S/N) ? ")
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
   eCLAS_PRO  = CODIGO
   eTIPO_PRO  = TIPO
   eDESC_PRO  = DESCRICAO
   eCOD_CONTA = COD_CONTA
   EXIT
ENDDO
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
      PLQUA("01")
      PPERG("Deseja Selecionar "+sisGRUPO1+" (S/N) ? ")
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
         PLQUA("01")
         PPERG("Deseja Selecionar "+sisUNID1+" (S/N) ? ")
         IF sisRESP = "S"
            @ 07,02 SAY sisUNID1+"   : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
               PMSGR("Atená∆o !",3)
               PMENSAG("N∆o H† Imoveis Dispon°veis !")
               PPARA()
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
      PLQUA("01")
      PPERG("Deseja Selecionar "+sisGRUPO2+" (S/N) ? ")
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
         PLQUA("01")
         PPERG("Deseja Selecionar "+sisUNID2+" (S/N) ? ")
         IF sisRESP = "S"
            @ 07,02 SAY sisUNID2+"  : "
            SELE 1
            SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. IIF(STATUS = 0,(PROPRIET_1 <> 1),(PROPRIET_1 <> 1 .OR. PROPRIET_2 <> 1))
            GOTO TOP
            IF EOF()
               PLQUA("01")
               PMSGR("Atená∆o !",3)
               PMENSAG("N∆o H† Imoveis Dispon°veis !")
               PPARA()
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
      PDESENV()
ENDCASE
eDATA_INI   = CTOD("")
eDATA_FIM   = CTOD("")
DO WHILE .T.
   @ 08,02 SAY "Vendas de : " GET eDATA_INI
   @ 08,27 SAY "Ate : "       GET eDATA_FIM
   READ
   IF eDATA_INI = CTOD("") .OR. eDATA_FIM = CTOD("")
      PLQUA("01")
      PMSGR("Datas Inv†lidas !",3)
      LOOP
   ELSE
      IF eDATA_INI > eDATA_FIM
         PLQUA("01")
         PMSGR("Data Inicio > Que Final !",3)
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDIF
ENDDO
eNOME_ARQ = SPACE(30)
eNOME_ARQ = "C:\EXPCEF          "
DO WHILE .T.
   PLQUA("01")
   @ 23,00 SAY "CONFIRME O DESTINO DE GERAÄ«O DO RELAT‡RIO : " GET eNOME_ARQ
   READ
   eNOME_ARQ = ALLTRIM(eNOME_ARQ)
   PLQUA("01")
   PMENSAG("Tecle ENTER para iniciar.")
   PPARA()
   IF eNOME_ARQ = SPACE(30)
     LOOP
   ELSE
     EXIT
   ENDIF
ENDDO
eARQ = ALLTRIM(eNOME_ARQ)+"1.TXT"
* Inicio da Geraá∆o dos arquivos
PLQUA("01")
PMENSAG("Preparando Geraá∆o de Arquivos, Tecle [ENTER]")
SET EXCLUSIVE ON
eARQ  = ALLTRIM(eARQ)
* Preparaá∆o dos Dados para o arquivo Clientes
SET PRINT ON
SET PRINTER TO &eARQ
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
   IF eDATA_VENDA >= eDATA_INI .AND. eDATA_VENDA <= eDATA_FIM .AND. eSTATUS = 0
      IF eSTATUS = 0
         SELE 1
         ePROPRIET_1 = PROPRIET_1
         SELE 5
         SEEK ePROPRIET_1
         eNOME    = CLIENTE
         eSEXO    = SEXO
         eCPF_CGC = CPF_CGC
         eEMAIL   = MAIL_PROP
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
         eCPF_CGC = REPLICATE("0",15-LEN(ALLTRIM(eAUX_CGC)))+ALLTRIM(eAUX_CGC)
         eDT_NASC = SUBSTR(DTOC(NASCIMENTO),1,2)+SUBSTR(DTOC(NASCIMENTO),4,2)+SUBSTR(DTOC(NASCIMENTO),7,4)
         IF OPCAO = "R"
            eEND       = END_R
            eBAIRRO    = BAIRRO_R
            eCIDADE    = CIDADE_R
            eESTADO    = ESTADO_R
            eCEP       = CEP_R
            ePROFISSAO = PROFISSAO
            eDDD       = SUBSTR(TEL_R_1,3,2)
            eTEL       = SUBSTR(TEL_R_1,5,8)
         ELSE
            eEND       = END_C
            eBAIRRO    = BAIRRO_C
            eCIDADE    = CIDADE_C
            eESTADO    = ESTADO_C
            eCEP       = CEP_C
            ePROFISSAO = PROFISSAO
            eDDD       = SUBSTR(TEL_C_1,3,2)
            eTEL       = SUBSTR(TEL_C_1,5,8)
         ENDIF
         SELE 1

         ? SUBSTR(eNOME,1,40)+IIF(eSEXO = "M" .OR. eSEXO = "F","1","2")+SUBSTR(eCPF_CGC,1,15)+SUBSTR(eEND,1,40)+SUBSTR(eBAIRRO,1,15)+SUBSTR(eCEP,1,8)+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+SUBSTR(ePROFISSAO,1,15)+eDDD+eTEL+IIF(eSEXO = "M" .OR. eSEXO = "F",eDT_NASC,SPACE(8))+SUBSTR(eEMAIL,1,50)

      ENDIF
   ENDIF
   SELE 1
   SKIP
ENDDO

SET PRINT OFF
SET EXCLUSIVE ON
eARQ  = ALLTRIM(eNOME_ARQ)+"2.TXT"
* Preparaá∆o dos Dados para o arquivo Imovel
SET PRINT ON
SET PRINTER TO &eARQ
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
   eDATA_VENDA = DATA_VENDA
   ePRAZO_FIN  = (ULT_VENC - PRI_VENC) / 30
   eVALOR_FIN  = VLR_PARCEL+INTERMEDIA+SINAL
   eSALDO_FIN  = 0
   ePERIODO    = PERIODO
   ePERIODO2   = IIF(ePERIODO="Mensal","06","09")
   eDT_VENDA2  = SUBSTR(DTOC(eDATA_VENDA),1,2)+SUBSTR(DTOC(eDATA_VENDA),4,2)+SUBSTR(DTOC(eDATA_VENDA),7,4)

   * Abrir arquivo de parcelas
   SELE 8
   eIND_CLAS = STR(eCLAS_PRO,5)
   eIND_QUAD = eQUADRA
   eZERO = "000"
   eARQP = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
   USE &eARQP INDEX &eIND1,&eIND2,&eIND3
   SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. ((DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .OR. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)) .AND. ATIVA
   GOTO TOP
   DO WHILE .NOT. EOF()
      IF DATA_PGTO = CTOD("")
         eSALDO_FIN  = eSALDO_FIN + VLR_PARC
      ENDIF
      SELE 8
      SKIP
   ENDDO
   SELE 7
   IF eDATA_VENDA >= eDATA_INI .AND. eDATA_VENDA <= eDATA_FIM .AND. eSTATUS = 0
      SELE 4
      GOTO TOP
      SEEK STR(eCLAS_PRO,5)+eQUADRA
      eNOME_BLOCO = DESCRICAO
      SELE 1
      ePROPRIET_1 = PROPRIET_1
      SELE 5
      SEEK ePROPRIET_1
      eNOME    = CLIENTE
      eSEXO    = SEXO
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
      eCPF_CGC = REPLICATE("0",15-LEN(ALLTRIM(eAUX_CGC)))+ALLTRIM(eAUX_CGC)

      eDT_NASC = SUBSTR(DTOC(NASCIMENTO),1,2)+SUBSTR(DTOC(NASCIMENTO),4,2)+SUBSTR(DTOC(NASCIMENTO),7,4)
 
      SELE 1
      eAREA_UTIL  = METRAGEM_1
      eAREA_TOTAL = METRAGEM_1

      eVLR_FINA  = INT(ROUND((eVALOR_FIN*100),0))
      eTAM_FINA  = LENNUM(eVLR_FINA)
      eVLR_FINA  = STR(eVLR_FINA,eTAM_FINA,0)
      eVLR_FINA  = ALLTRIM(eVLR_FINA)
      eVLR_FINA  = REPLICATE("0",15-eTAM_FINA)+ALLTRIM(eVLR_FINA)
       


      ? SUBSTR(eNOME_BLOCO,1,10)+SPACE(5)+eNUM_IMOVEL+"11"+STR(eAREA_UTIL,6)+STR(eAREA_TOTAL,6)+SUBSTR(eCPF_CGC,1,15)+"2"+SPACE(8)+STR(ePRAZO_FIN,3)+eVLR_FINA+"00"+STR(eSALDO_FIN,15)+"00"+eDT_VENDA2+ePERIODO2+SUBSTR(eNOME,1,40)+IIF(eSEXO = "M" .OR. eSEXO = "F","1","2")+SUBSTR(eCPF_CGC,1,15)


   ENDIF
   SELE 1
   SKIP
ENDDO
SET PRINT OFF

SET PRINT OFF
SET EXCLUSIVE ON
eARQ  = ALLTRIM(eNOME_ARQ)+"3.TXT"
* Preparaá∆o dos Dados para o arquivo Parcelas
SET PRINT ON
SET PRINTER TO &eARQ
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
   eDATA_VENDA = DATA_VENDA
   ePRAZO_FIN  = (ULT_VENC - PRI_VENC) / 30
   eVALOR_FIN  = VLR_PARCEL+INTERMEDIA+SINAL
   eSALDO_FIN  = 0
   ePERIODO    = PERIODO
   ePERIODO2   = IIF(ePERIODO="Mensal","06","09")
   eDT_VENDA2  = SUBSTR(DTOC(eDATA_VENDA),1,2)+SUBSTR(DTOC(eDATA_VENDA),4,2)+SUBSTR(DTOC(eDATA_VENDA),7,4)

   IF eDATA_VENDA >= eDATA_INI .AND. eDATA_VENDA <= eDATA_FIM .AND. eSTATUS = 0
      SELE 4
      GOTO TOP
      SEEK STR(eCLAS_PRO,5)+eQUADRA
      eNOME_BLOCO = DESCRICAO
      SELE 1
      ePROPRIET_1 = PROPRIET_1
      SELE 5
      SEEK ePROPRIET_1
      eNOME    = CLIENTE
      eSEXO    = SEXO
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
      eCPF_CGC = REPLICATE("0",15-LEN(ALLTRIM(eAUX_CGC)))+ALLTRIM(eAUX_CGC)

      eDT_NASC = SUBSTR(DTOC(NASCIMENTO),1,2)+SUBSTR(DTOC(NASCIMENTO),4,2)+SUBSTR(DTOC(NASCIMENTO),7,4)
 
      SELE 1
      eAREA_UTIL  = METRAGEM_1
      eAREA_TOTAL = METRAGEM_1

      * Abrir arquivo de parcelas
      SELE 8
      eIND_CLAS = STR(eCLAS_PRO,5)
      eIND_QUAD = eQUADRA
      eZERO = "000"
      eARQP = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND1 = "IG"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND2 = "IV"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      eIND3 = "IP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_CLAS))))+ALLTRIM(eIND_CLAS)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(eIND_QUAD))))+ALLTRIM(eIND_QUAD) 
      USE &eARQP INDEX &eIND1,&eIND2,&eIND3
      SET FILTER TO CLASSE = STR(eCLAS_PRO,5) .AND. QUADRA = eQUADRA .AND. NUM_IMOVEL = eNUM_IMOVEL .AND. PARTE = ePARTE .AND. ((DATA_VENC >= eDATA_INI .AND. DATA_VENC <= eDATA_FIM) .OR. (DATA_PGTO >= eDATA_INI .AND. DATA_PGTO <= eDATA_FIM)) .AND. ATIVA
      GOTO TOP
      DO WHILE .NOT. EOF()
         eNUM_PARC  = NUM_PARC
         eDATA_VENC = SUBSTR(DTOC(DATA_VENC),1,2)+SUBSTR(DTOC(DATA_VENC),4,2)+SUBSTR(DTOC(DATA_VENC),7,4)
         eDATA_PGTO = SUBSTR(DTOC(DATA_PGTO),1,2)+SUBSTR(DTOC(DATA_PGTO),4,2)+SUBSTR(DTOC(DATA_PGTO),7,4)
         eDATA_EMIS = SUBSTR(sisDATA,1,2)+SUBSTR(sisDATA,4,2)+SUBSTR(sisDATA,7,4)
         eVLR_PGTO  = STR(VLR_PGTO,15)+"00"
         eTIPO_PARC = IIF(TIPO_PARC="N".OR.TIPO_PARC="R","2",IIF(TIPO_PARC="I","3",IIF(TIPO_PARC="S","1","0")))

         eVLR_PARC  = INT(ROUND((VLR_PARC*100),0))
         eTAM_PARC  = LENNUM(VLR_PARC)
         eVLR_PARC  = STR(eVLR_PARC,eTAM_PARC,0)
         eVLR_PARC  = ALLTRIM(eVLR_PARC)
         eVLR_PARC  = REPLICATE("0",15-eTAM_PARC)+ALLTRIM(eVLR_PARC)

         eVLR_PGTO  = INT(ROUND((VLR_PGTO*100),0))
         eTAM_PGTO  = LENNUM(eVLR_PGTO)
         eVLR_PGTO  = STR(eVLR_PGTO,eTAM_PGTO,0)
         eVLR_PGTO  = ALLTRIM(eVLR_PGTO)
         eVLR_PGTO  = REPLICATE("0",15-eTAM_PGTO)+ALLTRIM(eVLR_PGTO)



*        ? SUBSTR(eCPF_CGC,1,15)+SUBSTR(eNOME_BLOCO,1,10)+SPACE(5)+eNUM_IMOVEL+eNUM_PARC+SPACE(11)+"A02330100"+ALLTRIM(eDATA_VENC)+ALLTRIM(eDATA_EMIS)+eVLR_PARC+REPLICATE("0",2+8+15+15)+"00"+SPACE(8+15)+"3"+SPACE(8+15+40+40+40+40+40+40)+eTIPO_PARC+eDATA_PGTO+eVLR_PGTO

         ? SUBSTR(eCPF_CGC,1,15)+" "+SUBSTR(eNOME_BLOCO,1,10)+" "+SPACE(5)+eNUM_IMOVEL+" "+eNUM_PARC+" "+SPACE(11)+" "+"A"+" "+"02"+" "+"3"+" "+"30"+" "+"1"+" "+"00"+" "+ALLTRIM(eDATA_VENC)+" "+ALLTRIM(eDATA_EMIS)+" "+eVLR_PARC+" "+"00"+" "+"00000000"+" "+REPLICATE("0",15)+" "+REPLICATE("0",15)+"00"+" "+SPACE(8)+" "+REPLICATE("0",15)+" "+"3"+" "+REPLICATE("0",8)+" "+REPLICATE("0",15)+" "+REPLICATE(" ",40)+" "+REPLICATE(" ",40)+" "+REPLICATE(" ",40)+" "+REPLICATE(" ",40)+" "+REPLICATE(" ",40)+" "+REPLICATE(" ",40)+" "+eTIPO_PARC+" "+ALLTRIM(eDATA_PGTO)+" "+eVLR_PGTO

         SELE 8
         SKIP
      ENDDO
   ENDIF
   SELE 1
   SKIP
ENDDO
SET PRINT OFF



SET DEVICE TO SCREEN
CLOSE DATABASES
RESTORE SCREEN FROM TELA05
RETURN NIL
*************************  fim  *********************************
