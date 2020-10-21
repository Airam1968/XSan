*********************************************************************
* Sistema     : GestÆo de Financiamentos                        
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : REGISTRO.PRG
* Data        : 01/01/99
* Objetivo    : GERACAO DO REGISTRO TRANSA‡Æo PARA ARQUIVO REMESSA
* PRG"s       : ---
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION REGISTRO()
DO CASE
   CASE eNUM_BANCO = "001"

        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"
        ENDIF

        eCOD_SEQ   = eSEQUENCIA
        eSEQUENCIA = eSEQUENCIA + 1
        eCOD_SEQ   = STRZERO(eCOD_SEQ,10)

        eTEM_ID7 = ALLTRIM(eNUM_PARC)
        eTAM_ID7 = LEN(eTEM_ID7)
        eTEM_ID7 = SUBSTR(eTEM_ID7,eTAM_ID7,1)
        eID_7    = VAL(eTEM_ID7)
        eVAZIO   = "00"
        eID_7    = SUBSTR(eVAZIO,1,(2-LEN(ALLTRIM(STR(eID_7,5)))))
        eID_7    = SUBSTR(eID_7,LEN(eID_7)-2,2)

        ? "702"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,1,5)+SUBSTR(eCOD_CC,1,9)+ALLTRIM(eCAIXA_POS)+eESP_IDENT+eINDENT+ALLTRIM(eCAIXA_POS)+eCOD_SEQ+REPLICATE("0",6)+;
        "AI 0190"+SPACE(11)+"17001"+"00"+eINDENT+eID_7+eDT_VENC+eVLR_PARCE+"0010000001A"+eDT_EMIS+REPLICATE("0",62)+;
        eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,37)+SPACE(3)+SUBSTR(eENDERECO,1,37)+SPACE(15)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        SUBSTR(eMSG,1,40)+SPACE(3)+eREG_SEQ


*        ? "702"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,1,5)+SUBSTR(eCOD_CC,1,9)+ALLTRIM(eCAIXA_POS)+eESP_IDENT+eINDENT+ALLTRIM(eCAIXA_POS)+eINDENT+"0"+STR(eID_7,1,1)+REPLICATE("0",8)+;
*        "AI 0190"+SPACE(11)+"1701"+eINDENT+"0"+STR(eID_7,1,1)+eDT_VENC+eVLR_PARCE+"0010000001A"+eDT_EMIS+REPLICATE("0",62)+;
*        eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,37)+SPACE(3)+SUBSTR(eENDERECO,1,37)+SPACE(15)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
*        SUBSTR(eMSG,1,40)+SPACE(3)+eREG_SEQ
   CASE eNUM_BANCO = "008"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,1,5)+"000000"+SUBSTR(eCOD_CC,1,9)+"00"+eESP_IDENT+eINDENT+SPACE(45)+;
        "301"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"008"+REPLICATE("0",5)+"01"+"N"+;
        eDT_EMIS+IF(PROTESTO,"0610","0001")+"0000000000033"+REPLICATE("0",45)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        SUBSTR(eMSG,1,40)+SPACE(2)+"1"+eREG_SEQ
   CASE eNUM_BANCO = "033"
        * CALCULO DO DAC
*        eAG_1 = VAL(SUBSTR(eCOD_AGENCI,2,1))
*        eAG_2 = VAL(SUBSTR(eCOD_AGENCI,3,1))
*        eAG_3 = VAL(SUBSTR(eCOD_AGENCI,4,1))
         eCOD_SEQ   = eSEQUENCIA
         eSEQUENCIA = eSEQUENCIA + 1
         eCOD_SEQ   = STRZERO(eCOD_SEQ,7)
*        eCC_1 = VAL(SUBSTR(eCOD_SEQ,1,1))
*        eCC_2 = VAL(SUBSTR(eCOD_SEQ,2,1))
*        eCC_3 = VAL(SUBSTR(eCOD_SEQ,3,1))
*        eCC_4 = VAL(SUBSTR(eCOD_SEQ,4,1))
*        eCC_5 = VAL(SUBSTR(eCOD_SEQ,5,1))
*        eCC_6 = VAL(SUBSTR(eCOD_SEQ,6,1))
*        eCC_7 = VAL(SUBSTR(eCOD_SEQ,7,1))
*        eID_1 = eAG_1 * 7
*        eID_2 = eAG_2 * 3
*        eID_3 = eAG_3 * 1
*        eID_4 = eCC_1 * 9
*        eID_5 = eCC_2 * 7
*        eID_6 = eCC_3 * 3
*        eID_7 = eCC_4 * 1
*        eID_8 = eCC_5 * 9
*        eID_9 = eCC_6 * 7
*        eID_0 = eCC_7 * 3


*        eDV   = 0
*        eDV   = eID_1 + eID_2 +eID_3 + eID_4 + eID_5 + eID_6 + eID_7 + eID_8 + eID_9 + eID_0
*        eTAM_DV = 0
*        eTAM_DV = LENNUM(eDV)
*        IF eTAM_DV > 1
*           eDV = SUBSTR(ALLTRIM(STR(eDV,5)),eTAM_DV,1)
*        ENDIF
*        eDV   = 10 - VAL(eDV)
*        IF eDV = 10
*           eDV = 0
*        ENDIF
*        eDV   = VAL(ALLTRIM(STR(eDV,5)))

*        eCODIGO = ALLTRIM(STR(eAG_1,1))+ALLTRIM(STR(eAG_2,1))+ALLTRIM(STR(eAG_3,1))
*        eCODIGO = ALLTRIM(eCODIGO+ALLTRIM(STR(eCC_1,1))+ALLTRIM(STR(eCC_2,1))+ALLTRIM(STR(eCC_3,1))+ALLTRIM(STR(eCC_4,1))+ALLTRIM(STR(eCC_5,1))+ALLTRIM(STR(eCC_6,1))+ALLTRIM(STR(eCC_7,1))+ALLTRIM(STR(eDV,1)))

        eCODIGO = ALLTRIM(eCOD_SEQ)

        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,2,3)+SUBSTR(eCOD_CC,1,8)+SPACE(9)+eESP_IDENT+eINDENT+SUBSTR(eCOD_AGENCI,2,3)+eCODIGO+;
        SPACE(35)+"101"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"033"+REPLICATE("0",5)+"01"+"A"+;
        eDT_EMIS+IIF(PROTESTO,"06","07")+"58"+"0000000000033"+REPLICATE("0",45)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,30)+"- No:"+eNUM_PARC+;                    
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;                               
        eMSG+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+SPACE(1)+eREG_SEQ

   CASE eNUM_BANCO = "104"
            IF eASSUNTO = "SIGCB"   
            	* SIGCB
		eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))            
		eCPF_CGC     = REPLICATE("0",14-eTAM_CPF_CGC)+eCPF_CGC
		eINDENT = REPLICATE(" ",25-LEN(ALLTRIM(eINDENT)))+ALLTRIM(eINDENT)
		* MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
		IF eMSG_AUTO
		   eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
		ENDIF
		? "102"+sisCGC_CPF+eCOD_AGENCI+eCOMPLEM+"11"+"00"+eINDENT+"11"+REPLICATE("0",15)+SPACE(3)+;
		SUBSTR(eMSG,1,30)+"01"+"01"+ALLTRIM(eINDENT)+SPACE(2)+eDT_VENC+eVLR_PARCE+"104"+REPLICATE("0",5)+"01"+"A"+;
		eDT_EMIS+IIF(PROTESTO,"01","02")+"00"+REPLICATE("0",13+6+13+13+13)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
		SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
		eDT_MULT+REPLICATE("0",10)+REPLICATE(" ",22)+"00"+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+"1"+eREG_SEQ  
		*
            ELSE
            	* SICOB
		eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))            
		eCPF_CGC     = REPLICATE("0",14-eTAM_CPF_CGC)+eCPF_CGC
		eINDENT = REPLICATE(" ",25-LEN(ALLTRIM(eINDENT)))+ALLTRIM(eINDENT)
		* MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
		IF eMSG_AUTO
		   eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
		ENDIF
		? "102"+sisCGC_CPF+eCOD_AGENCI+eCOD_CC+SPACE(2)+"00"+eINDENT+REPLICATE("0",11)+SPACE(3)+;
		SUBSTR(eMSG,1,30)+"11"+"01"+ALLTRIM(eINDENT)+SPACE(2)+eDT_VENC+eVLR_PARCE+"104"+"00000"+"01"+"A"+;
		eDT_EMIS+IIF(PROTESTO,"01","02")+"00"+REPLICATE("0",13+6+13+13+13)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,35)+;
		SPACE(5)+SUBSTR(eENDERECO,1,35)+SPACE(5)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
		eDT_MULT+REPLICATE("0",10)+REPLICATE(" ",22)+"00"+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+"1"+eREG_SEQ             
            ENDIF   

   CASE eNUM_BANCO = "151"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))            
        eCPF_CGC     = REPLICATE("0",14-eTAM_CPF_CGC)+eCPF_CGC
        eINDENT = REPLICATE(" ",25-LEN(ALLTRIM(eINDENT)))+ALLTRIM(eINDENT)
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "15100013"+SUBSTR(eREG_SEQ,2,5)+"P 0100"+eCOD_AGENCI+"0000"+eCOD_CC+SPACE(5)+"098870"+"0000000000"+"052120"+;
        SPACE(3)+SUBSTR(eINDENT,13,13)+eDT_VENC+"00"+eVLR_PARCE+"00000002A"+eDT_EMIS+"3"+SPACE(8)+"00000000000100001"+REPLICATE("0",23)+SPACE(30)+;
        ALLTRIM(eINDENT)+" "+IIF(PROTESTO,ePRAZO_PRO,"99")+SPACE(4)+"00"+SPACE(3)+"00"+SPACE(4)

        * Numero Sequencial do Registro
        eNUM_SEQ = eNUM_SEQ + 1
        eTAM_SEQ = LENNUM(eNUM_SEQ)
        eREG_SEQ = REPLICATE("0",6-eTAM_SEQ)+STR(eNUM_SEQ,eTAM_SEQ,0)

        ? "15100013"+eREG_SEQ+"Q 0110"+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,15)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+"20"+sisCGC_CPF+SUBSTR(eCLIENTE,1,40)+SPACE(31)
   CASE eNUM_BANCO = "215"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+eCOD_AGENCI+"02"+SUBSTR(eCOD_CC,1,8)+SPACE(6)+eESP_IDENT+eINDENT+REPLICATE("0",16)+;
        "00"+SPACE(27)+"101"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"215"+REPLICATE("0",5)+"15"+"A"+;
        eDT_EMIS+IIF(PROTESTO,"16","20")+"00"+REPLICATE("0",58)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        IIF(LEN(ALLTRIM(eENDERECO)) > 30,SUBSTR(eENDERECO,1,40),SUBSTR(eENDERECO,1,30)+SUBSTR(eBAIRRO,1,10))+IIF(PROTESTO,ePRAZO_PRO,"00")+ePRAZO_DEV+SPACE(8)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        eMSG+"99"+" "+eREG_SEQ
   CASE eNUM_BANCO = "230"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,2,3)+SUBSTR(eCOD_CC,1,10)+SPACE(7)+eESP_IDENT+eINDENT+REPLICATE("0",13)+;
        SPACE(7)+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+SPACE(23)+"101"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"230"+REPLICATE("0",5)+"01"+"A"+;
        eDT_EMIS+IIF(PROTESTO,"04","01")+"00"+REPLICATE("0",13)+REPLICATE("0",45)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        eMSG+REPLICATE(" ",2)+"9"+eREG_SEQ
   CASE eNUM_BANCO = "237"
        * CALCULO DO DAC BASE 11 CARTEIRA No. 9
        eCA_1 = 0
        eCA_2 = 9
        eID_1 = VAL(SUBSTR(eINDENT,2,1))
        eID_2 = VAL(SUBSTR(eINDENT,3,1))
        eID_3 = VAL(SUBSTR(eINDENT,4,1))
        eID_4 = VAL(SUBSTR(eINDENT,5,1))
        eID_5 = VAL(SUBSTR(eINDENT,6,1))
        eID_6 = VAL(SUBSTR(eINDENT,7,1))
        eID_7 = 0
        eID_8 = 0
        eID_9 = 0
        eID_10= 0
        eID_11= 0

        eTEM_ID = ALLTRIM(eNUM_PARC)
        eTAM_ID = LEN(eTEM_ID)
        eTEM_OR = eTEM_ID
        IF eTAM_ID = 1
           eTEM_ID = SUBSTR(eTEM_OR,eTAM_ID,1)
           eID_7   = 0
           eID_8   = 0
           eID_9   = VAL(eTEM_ID)
        ELSE
           IF eTAM_ID = 2
              eTEM_ID = SUBSTR(eTEM_OR,eTAM_ID,1)
              eID_7   = 0
              eID_8   = 0
              eID_9   = VAL(eTEM_ID)
              eTEM_ID = SUBSTR(eTEM_OR,eTAM_ID-1,1)
              eID_8   = VAL(eTEM_ID)
           ELSE
              eTEM_ID = SUBSTR(eTEM_OR,eTAM_ID,1)
              eID_7   = 0
              eID_8   = 0
              eID_9   = VAL(eTEM_ID)
              eTEM_ID = SUBSTR(eTEM_OR,eTAM_ID-1,1)
              eID_8   = VAL(eTEM_ID)
              eTEM_ID = SUBSTR(eTEM_OR,eTAM_ID-2,1)
              eID_7   = VAL(eTEM_ID)
           ENDIF
        ENDIF

        eAG_1  = eCA_1  * 2
        eAG_2  = eCA_2  * 7
        eAG_3  = eID_1  * 6
        eAG_4  = eID_2  * 5
        eAG_5  = eID_3  * 4
        eAG_6  = eID_4  * 3
        eAG_7  = eID_5  * 2
        eAG_8  = eID_6  * 7
        eAG_9  = eID_7  * 6
        eAG_10 = eID_8  * 5
        eAG_11 = eID_9  * 4
        eAG_12 = eID_10 * 3
        eAG_13 = eID_11 * 2

        eDV   = 0
        eDV   = eAG_1 + eAG_2 +eAG_3 + eAG_4 + eAG_5 + eAG_6 + eAG_7 + eAG_8 + eAG_9 + eAG_10 +eAG_11 + eAG_12 + eAG_13

        IF MOD(eDV,11) = 1
           eDV = "P"
        ELSE
           IF MOD(eDV,11) = 0
              eDV = "0"
           ELSE
              eDV = 11 - MOD(eDV,11)
              eDV = ALLTRIM(STR(eDV,5))
           ENDIF
        ENDIF

        eDAC = SPACE(12)
        eDAC = STR(eID_1,1)+STR(eID_2,1)+STR(eID_3,1)+STR(eID_4,1)+STR(eID_5,1)+STR(eID_6,1)+ALLTRIM(STR(eID_7,1))+ALLTRIM(STR(eID_8,1))+STR(eID_9,1)+STR(eID_10,1)+STR(eID_11,1)+eDV

        * Bradesco
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "1"+REPLICATE("0",5+1+5+7+1)+"00090"+SUBSTR(eCOD_AGENCI,1,5)+SUBSTR(eCOD_CC,1,8)+eESP_IDENT+eINDENT+"000"+REPLICATE("0",5)+eDAC+REPLICATE("0",10)+"1 "+SPACE(10)+;
        " 0  "+"01"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+REPLICATE("0",8)+"01A"+eDT_EMIS+IIF(PROTESTO,"06"+ePRAZO_PRO,"0000")+REPLICATE("0",(13+6+13+13+13))+;
        eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+SUBSTR(eENDERECO,1,40)+SPACE(12)+eCEP+SPACE(60)+eREG_SEQ


        * Numero Sequencial do Registro
        eNUM_SEQ = eNUM_SEQ + 1
        eTAM_SEQ = LENNUM(eNUM_SEQ)
        eREG_SEQ = REPLICATE("0",6-eTAM_SEQ)+STR(eNUM_SEQ,eTAM_SEQ,0)

        ? "2"+eMSG+SPACE(40)+eMSG_2+SPACE(40)+eMSG_3+SPACE(40)+eMSG_4+SPACE(40)+SPACE(45)+"0090"+SUBSTR(eCOD_AGENCI,1,5)+"99179066"+eDAC+eREG_SEQ
   CASE eNUM_BANCO = "275"
        *
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+"0"+SUBSTR(eCOD_AGENCI,1,4)+"0"+SUBSTR(eCOD_CC,1,7)+SPACE(7)+eINDENT+SPACE(17)+"0"+eINDENT+SPACE(24)+REPLICATE("0",9)+;
        SPACE(1)+"2 501"+"  "+eINDENT+eDT_VENC+eVLR_PARCE+"275"+REPLICATE("0",5)+"99"+"A"+eDT_EMIS+"0000"+REPLICATE("0",58)+;
        eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        SUBSTR(eMSG,1,40)+SPACE(1)+"77"+eREG_SEQ
        *
   CASE eNUM_BANCO = "291"
        * CALCULO DO DAC
        eID_1 = VAL(SUBSTR(eINDENT,2,1))
        eID_2 = VAL(SUBSTR(eINDENT,3,1))
        eID_3 = VAL(SUBSTR(eINDENT,4,1))
        eID_4 = VAL(SUBSTR(eINDENT,5,1))
        eID_5 = VAL(SUBSTR(eINDENT,6,1))
        eID_6 = VAL(SUBSTR(eINDENT,7,1))

        eTEM_ID7 = ALLTRIM(eNUM_PARC)
        eTAM_ID7 = LEN(eTEM_ID7)
        eTEM_ID7 = SUBSTR(eTEM_ID7,eTAM_ID7,1)
        eID_7    = VAL(eTEM_ID7)            

*       eID_7 = VAL(SUBSTR(ALLTRIM(eNUM_PARC),1,1))

        eAG_1 = eID_1 * 80
        eAG_2 = eID_2 * 70
        eAG_3 = eID_3 * 60
        eAG_4 = eID_4 * 50
        eAG_5 = eID_5 * 40
        eAG_6 = eID_6 * 30
        eAG_7 = eID_7 * 20

        eDV   = eAG_1 + eAG_2 +eAG_3 + eAG_4 + eAG_5 + eAG_6 +eAG_7
        eDV   = IIF(MOD(eDV,11) = 10,0,MOD(eDV,11))
        eDV   = ALLTRIM(STR(eDV,5))

        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC

        eINDENT = SUBSTR(eINDENT,1,7)+ALLTRIM(STR(eID_7))

        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "101"+sisCGC_CPF+SPACE(10)+SUBSTR(eCOD_AGENCI,2,3)+SUBSTR(eCOD_CC,1,7)+eINDENT+eESP_IDENT+SPACE(12)+SUBSTR(eINDENT,2,7)+eDV+;
        SPACE(25)+"101"+"00"+eINDENT+eDT_VENC+eVLR_PARCE+"291"+REPLICATE("0",5)+"01"+"A"+;
        eDT_EMIS+ePRAZO_DEV+ePRAZO_PRO+REPLICATE("0",58)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        REPLICATE(" ",43)+eREG_SEQ
   CASE eNUM_BANCO = "341"
        DO CASE
        CASE eCOD_CART = "112"
           eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
           eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
           * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
           IF eMSG_AUTO
              eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
           ENDIF
           eCOD_BANCO = "00000000"
           ? "102"+sisCGC_CPF+eCOD_AGENCI+eCOD_CC+SPACE(8)+eINDENT+eESP_IDENT+eCOD_BANCO+REPLICATE("0",13)+;
           "112"+SPACE(21)+"I"+"01"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"341"+SPACE(5)+"06"+"A"+;
           eDT_EMIS+IIF(PROTESTO,"09","92")+REPLICATE("0",60)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
           SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
           REPLICATE(" ",34)+eDT_VENC+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+" "+eREG_SEQ
        CASE eCOD_CART = "121"
           eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
           eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
           * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
           IF eMSG_AUTO
              eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
           ENDIF
           ? "102"+sisCGC_CPF+eCOD_AGENCI+eCOD_CC+SPACE(25)+eINDENT+REPLICATE("0",8)+REPLICATE("0",13)+eCOD_BANCO+;
           "121"+SPACE(21)+"I"+"01"+"00"+eINDENT+eDT_VENC+eVLR_PARCE+"341"+SPACE(5)+"06"+"A"+;
           eDT_EMIS+IIF(PROTESTO,"09","92")+REPLICATE("0",60)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
           SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
           REPLICATE(" ",34)+eDT_VENC+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+" "+eREG_SEQ
        CASE eCOD_CART = "175" .OR. eCOD_CART = "179"
           eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
           eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
           * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
           IF eMSG_AUTO
              eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
           ENDIF
           eAG_1 = 0
           eAG_2 = 0
           eAG_3 = 0
           eAG_4 = 0
           eCC_1 = 0
           eCC_2 = 0
           eCC_3 = 0
           eCC_4 = 0
           eCC_5 = 0
           eCA_1 = 0
           eCA_2 = 0
           eCA_3 = 0
           eID_1 = 0
           eID_2 = 0
           eID_3 = 0
           eID_4 = 0
           eID_5 = 0
           eID_6 = 0
           eID_7 = 0
           eID_8 = 0

           * CALCULO DO DAC
           eAG_1 = VAL(SUBSTR(eCOD_AGENCI,1,1))
           eAG_2 = VAL(SUBSTR(eCOD_AGENCI,2,1))
           eAG_3 = VAL(SUBSTR(eCOD_AGENCI,3,1))
           eAG_4 = VAL(SUBSTR(eCOD_AGENCI,4,1))
           eCC_1 = VAL(SUBSTR(eCOD_CC,3,1))
           eCC_2 = VAL(SUBSTR(eCOD_CC,4,1))
           eCC_3 = VAL(SUBSTR(eCOD_CC,5,1))
           eCC_4 = VAL(SUBSTR(eCOD_CC,6,1))
           eCC_5 = VAL(SUBSTR(eCOD_CC,7,1))
           IF eCOD_CART = "175"
              eCA_1 = 1
              eCA_2 = 7
              eCA_3 = 5
           ELSE
              eCA_1 = 1
              eCA_2 = 7
              eCA_3 = 9
           ENDIF
           eID_1 = VAL(SUBSTR(eINDENT,1,1))
           eID_2 = VAL(SUBSTR(eINDENT,2,1))
           eID_3 = VAL(SUBSTR(eINDENT,3,1))
           eID_4 = VAL(SUBSTR(eINDENT,4,1))
           eID_5 = VAL(SUBSTR(eINDENT,5,1))
           eID_6 = VAL(SUBSTR(eINDENT,6,1))
           eID_7 = VAL(SUBSTR(eINDENT,7,1))
           eID_8 = VAL(SUBSTR(eINDENT,8,1))


           eAG_1 = eAG_1 * 1
           eAG_2 = eAG_2 * 2
           eAG_3 = eAG_3 * 1
           eAG_4 = eAG_4 * 2
           eCC_1 = eCC_1 * 1
           eCC_2 = eCC_2 * 2
           eCC_3 = eCC_3 * 1
           eCC_4 = eCC_4 * 2
           eCC_5 = eCC_5 * 1
           eCA_1 = eCA_1 * 2
           eCA_2 = eCA_2 * 1
           eCA_3 = eCA_3 * 2
           eID_1 = eID_1 * 1
           eID_2 = eID_2 * 2
           eID_3 = eID_3 * 1
           eID_4 = eID_4 * 2
           eID_5 = eID_5 * 1
           eID_6 = eID_6 * 2
           eID_7 = eID_7 * 1
           eID_8 = eID_8 * 2


           eTAM_NUM = LENNUM(eAG_1)
           IF eTAM_NUM > 1
              e1_NUM = STR(eAG_1,eTAM_NUM,0)
              e2_NUM = STR(eAG_1,eTAM_NUM,0)
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eAG_1  = e1_NUM + e2_NUM
           ENDIF
           eTAM_NUM = LENNUM(eAG_2)
           IF eTAM_NUM > 1
              e1_NUM = STR(eAG_2,eTAM_NUM,0)
              e2_NUM = STR(eAG_2,eTAM_NUM,0)
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eAG_2  = e1_NUM + e2_NUM
           ENDIF
           eTAM_NUM = LENNUM(eAG_3)
           IF eTAM_NUM > 1
              e1_NUM = STR(eAG_3,eTAM_NUM,0)
              e2_NUM = STR(eAG_3,eTAM_NUM,0)
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eAG_3  = e1_NUM + e2_NUM
           ENDIF
           eTAM_NUM = LENNUM(eAG_4)
           IF eTAM_NUM > 1
              e1_NUM = STR(eAG_4,eTAM_NUM,0)
              e2_NUM = STR(eAG_4,eTAM_NUM,0)
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eAG_4  = e1_NUM + e2_NUM
           ENDIF
           eTAM_NUM = LENNUM(eCC_1)
           IF eTAM_NUM > 1
              e1_NUM = STR(eCC_1,eTAM_NUM,0)
              e2_NUM = STR(eCC_1,eTAM_NUM,0)
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCC_1  = e1_NUM + e2_NUM
           ENDIF
           eTAM_NUM = LENNUM(eCC_2)
           IF eTAM_NUM > 1
              e1_NUM = STR(eCC_2,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCC_2,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCC_2  = e1_NUM + e2_NUM
              *? eCC_2
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCC_3)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCC_3,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCC_3,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCC_3  = e1_NUM + e2_NUM
              *? eCC_3
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCC_4)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCC_4,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCC_4,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCC_4  = e1_NUM + e2_NUM
              *? eCC_4
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCC_5)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCC_5,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCC_5,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCC_5  = e1_NUM + e2_NUM
              *? eCC_5
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCA_1)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCA_1,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCA_1,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCA_1  = e1_NUM + e2_NUM
              *? eCA_1
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCA_2)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCA_2,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCA_2,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCA_2  = e1_NUM + e2_NUM
              *? eCA_2
              *?
           ENDIF
           eTAM_NUM = LENNUM(eCA_3)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eCA_3,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eCA_3,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eCA_3  = e1_NUM + e2_NUM
              *? eCA_3
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_1)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_1,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_1,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_1  = e1_NUM + e2_NUM
              *? eID_1
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_2)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_2,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_2,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_2  = e1_NUM + e2_NUM
              *? eID_2
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_3)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_3,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_3,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_3  = e1_NUM + e2_NUM
              *? eID_3
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_4)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_4,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_4,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_4  = e1_NUM + e2_NUM
              *? eID_4
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_5)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_5,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_5,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_5  = e1_NUM + e2_NUM
              *? eID_5
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_6)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_6,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_6,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_6  = e1_NUM + e2_NUM
              *? eID_6
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_7)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_7,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_7,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_7  = e1_NUM + e2_NUM
              *? eID_7
              *?
           ENDIF
           eTAM_NUM = LENNUM(eID_8)
           *? eTAM_NUM
           IF eTAM_NUM > 1
              e1_NUM = STR(eID_8,eTAM_NUM,0)
              *? e1_NUM
              e2_NUM = STR(eID_8,eTAM_NUM,0)
              *? e2_NUM
              e1_NUM = ALLTRIM(e1_NUM)
              e2_NUM = ALLTRIM(e2_NUM)
              e1_NUM = VAL(SUBSTR(e1_NUM,1,1))
              e2_NUM = VAL(SUBSTR(e2_NUM,2,1))
              eID_8  = e1_NUM + e2_NUM
              *? eID_8
              *?
           ENDIF

           eRESULTADO = 0
           eRESULTADO = eAG_1+eAG_2+eAG_3+eAG_4+eCC_1+eCC_2+eCC_3+eCC_4+eCC_5+eCA_1+eCA_2+eCA_3+eID_1+eID_2+eID_3+eID_4+eID_5+eID_6+eID_7+eID_8
*?eRESULTADO
           eDIVISAO   = 0
           eDIVISAO   = MOD(eRESULTADO,10)
*?eDIVISAO
           IF eDIVISAO <> 0
              eDAC       = 0
              eDAC       = STR((10 - eDIVISAO),1,1)
           ELSE
              eDAC       = "0"
           ENDIF
*?eDAC
           IF VAL(eDAC) < 0 .AND. VAL(eDAC) > 9
              SET DEFA TO SCREEN
			  PMENSAG("DAC com numeracao fora da faixa")
              PPARA()
              SET DEFA TO PRINT
           ENDIF

           ePAGAVEL  = "PAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO              "
           * Numero da Parcela aparece no sacador/avalista
           eTAM_PARC = LEN(eNUM_PARC)
           eNUM_PARC = REPLICATE("0",5-eTAM_PARC)+ALLTRIM(eNUM_PARC)
           eOBS_PARC = "        PARCELA NUMERO : "+eNUM_PARC

*          LINHA ANTERIOR eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+SUBSTR(eCLIENTE,1,30)+SPACE(4)+;

           ? "61"+eCOD_AGENCI+eCOD_CC+eCOD_CART+eINDENT+eDAC+"0"+SPACE(4)+eVLR_PARCE+eINDENT+"00"+eDT_VENC+"01A"+;
           eDT_EMIS+eTIPO_SAC+"0"+eCPF_CGC+SUBSTR(eCLIENTE,1,39)+SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+;
           eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+eOBS_PARC+SPACE(4)+;
           ePAGAVEL+SPACE(55)+"02"+sisCGC_CPF+SPACE(31)+eREG_SEQ

           * Registros de Mensagens
           eNUM_SEQ = eNUM_SEQ + 1
           eTAM_SEQ = LENNUM(eNUM_SEQ)
           eREG_SEQ = REPLICATE("0",6-eTAM_SEQ)+STR(eNUM_SEQ,eTAM_SEQ,0)

           ? "62"+eMSG+SPACE(29)+eMSG_2+SPACE(29)+eMSG_3+SPACE(29)+eMSG_4+SPACE(29)+eMSG_5+SPACE(29)+SPACE(47)+eREG_SEQ

        OTHERWISE
           SET PRINT OFF
           CLEAR
		   PMENSAG("Codigo de carteira incompativel, carteira 112,121,175 ou 179 aceitos")
           PPARA()
           PLQUA("01")
		   PMENSAG("Se sua carteira de cobranca e diferente destas, comunique a nossa Empresa.")
           PPARA()
           GOTO BOTTOM
           SET PRINT ON
        ENDCASE
   CASE eNUM_BANCO = "356"
        *
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+"0"+SUBSTR(eCOD_AGENCI,1,4)+"0"+SUBSTR(eCOD_CC,1,7)+SPACE(7)+eINDENT+SPACE(17)+"0"+eINDENT+SPACE(24)+REPLICATE("0",9)+;
        SPACE(1)+"2 501"+"  "+eINDENT+eDT_VENC+eVLR_PARCE+"356"+REPLICATE("0",5)+"99"+"A"+eDT_EMIS+"0000"+REPLICATE("0",58)+;
        eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        SUBSTR(eMSG,1,40)+SPACE(1)+"77"+eREG_SEQ
        *
   CASE eNUM_BANCO = "409"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,1,4)+SUBSTR(eCOD_CC,1,7)+SPACE(9)+eESP_IDENT+eINDENT+REPLICATE("0",11)+;
        SUBSTR(eMSG,1,30)+SPACE(4)+SUBSTR(eCOD_CART,2,1)+"01"+eINDENT+"00"+eDT_VENC+eVLR_PARCE+"409"+REPLICATE("0",5)+"01"+"A"+;
        eDT_EMIS+"03"+IF(PROTESTO,"06","07")+REPLICATE("0",58)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;                    
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        REPLICATE(" ",40)+ePRAZO_PRO+"0"+eREG_SEQ
   CASE eNUM_BANCO = "422"
        * CALCULO DO DAC
        IF eNUM_ATU <> 0
           eNUM_NOV = eNUM_ATU + 1
           eNUM_ATU = STR(INT(eNUM_ATU),8,0)
           eNU_1 = SUBSTR(ALLTRIM(eNUM_ATU),1,1)
           eNU_2 = SUBSTR(ALLTRIM(eNUM_ATU),2,1)
           eNU_3 = SUBSTR(ALLTRIM(eNUM_ATU),3,1)
           eNU_4 = SUBSTR(ALLTRIM(eNUM_ATU),4,1)
           eNU_5 = SUBSTR(ALLTRIM(eNUM_ATU),5,1)
           eNU_6 = SUBSTR(ALLTRIM(eNUM_ATU),6,1)
           eNU_7 = SUBSTR(ALLTRIM(eNUM_ATU),7,1)
           eNU_8 = SUBSTR(ALLTRIM(eNUM_ATU),8,1)

           eNU_1 = VAL(eNU_1) * 9
           eNU_2 = VAL(eNU_2) * 8
           eNU_3 = VAL(eNU_3) * 7
           eNU_4 = VAL(eNU_4) * 6
           eNU_5 = VAL(eNU_5) * 5
           eNU_6 = VAL(eNU_6) * 4
           eNU_7 = VAL(eNU_7) * 3
           eNU_8 = VAL(eNU_8) * 2

           eRESULTADO = 0
           eRESULTADO = eNU_1+eNU_2+eNU_3+eNU_4+eNU_5+eNU_6+eNU_7+eNU_8
       
           eRESTO = 0
           eRESTO = MOD(eRESULTADO,11)

           DO CASE
              CASE eRESTO = 0
                 eDAC = 1
              CASE eRESTO = 1
                 eDAC = 0
              OTHERWISE
                 eDAC   = 0
                 eDAC   = 11 - eRESTO
           ENDCASE

           eDAC   = SUBSTR(ALLTRIM(eNUM_ATU),1,8)+ALLTRIM(STR(INT(eDAC)))

           SELE 7
           IF F_BLOQR()
              REPLACE NUM_ATU WITH eNUM_NOV
              eNUM_ATU = eNUM_NOV
              UNLOCK
           ENDIF
           SELE 8
        ELSE
           eDAC   = "000000000"
        ENDIF
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SUBSTR(eCOD_AGENCI,1,5)+SUBSTR(eCOD_CC,1,9)+SPACE(6)+eESP_IDENT+eINDENT+eDAC+SPACE(30)+;
        "000"+SPACE(1)+IIF(PROTESTO,ePRAZO_PRO,"00")+"101"+eINDENT+"00"+;
        eDT_VENC+eVLR_PARCE+"422"+SPACE(5)+"01"+"A"+eDT_EMIS+IIF(PROTESTO,"06","03")+;
        IIF(PROTESTO,"10","00")+REPLICATE("0",58)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,10)+"  "+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        SUBSTR(eMSG,1,28)+"  "+SPACE(10)+SUBSTR(eNUM_ARQ,3,5)+eREG_SEQ
   CASE eNUM_BANCO = "424"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+eCOD_AGENCI+eCOD_CC+SPACE(9)+eESP_IDENT+eINDENT+SPACE(45)+;
        "901"+SPACE(2)+eINDENT+eDT_VENC+eVLR_PARCE+"424"+SPACE(5)+"01"+"A"+;
        eDT_EMIS+IIF(PROTESTO,"06","02")+REPLICATE("0",60)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+;
        REPLICATE(" ",40)+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+" "+eREG_SEQ
   CASE eNUM_BANCO = "479"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+SPACE(20)+eESP_IDENT+eINDENT+REPLICATE("0",20)+" R$  "+SPACE(20)+"30100"+eINDENT+;
        eDT_VENC+eVLR_PARCE+"479"+SPACE(5)+"DMA"+eDT_EMIS+IIF(PROTESTO,"01","03")+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+;
        REPLICATE("0",13+6+13+13+13)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+SUBSTR(eENDERECO,1,37)+SPACE(15)+;
        eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+eMSG+SPACE(3)+eREG_SEQ
   CASE eNUM_BANCO = "641"
        eTAM_CPF_CGC = LEN(ALLTRIM(eCPF_CGC))
        eCPF_CGC     = REPLICATE("0",14 - eTAM_CPF_CGC)+eCPF_CGC
        * MENSAGEM GERADA APARTIR DA MULTA DO FINANCIAMENTO.
        IF eMSG_AUTO
           eMSG = "MULTA ("+STR(eMULTA,2)+") E COMISSAO APOS VENC"+SPACE(10)
        ENDIF
        ? "102"+sisCGC_CPF+"000"+eCOD_CART+eCOD_AGENCI+eCOD_CC+eESP_IDENT+eINDENT+REPLICATE("0",20)+SPACE(25)+;
        "501"+SPACE(2)+eINDENT+eDT_VENC+eVLR_PARCE+"0000000001N"+eDT_EMIS+IIF(PROTESTO,"17","16")+REPLICATE("0",60)+eTIPO_SAC+eCPF_CGC+SUBSTR(eCLIENTE,1,40)+;
        SUBSTR(eENDERECO,1,40)+SUBSTR(eBAIRRO,1,12)+eCEP+SUBSTR(eCIDADE,1,15)+SUBSTR(eESTADO,1,2)+SUBSTR(eMSG,1,40)+IIF(PROTESTO,ePRAZO_PRO,ePRAZO_DEV)+"9"+eREG_SEQ
ENDCASE
eNUM_SEQ = eNUM_SEQ + 1
RETURN nil 
*************************  fim  *********************************
