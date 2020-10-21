*********************************************************************
* Sistema     : Gest∆o de Financiamentos
* Copyright   : Airam Miranda de Almeida
* Programador : Airam Miranda de Almeida
*********************************************************************
* Programa    : LEIT_DIS.PRG
* Data        : 01/01/99
* Objetivo    : GERACAO DE DISQUETE COM EMPREENDIMENTO
* PRG"s       : INICIO
* DBF"s       : ---
* Alteracao   :
************************* inicio ************************************
FUNCTION LEIT_DIS()
PLQUA("01")
   PMENSAG("Insira um Disco Formatado na Unidade A: e Aguarde")
   PPARA()
   SET EXCLUSIVE ON
   SELE 1
   USE CONTATO
   GOTO TOP
   COPY TO A:\CONTATO
   USE
   USE CLAS_PRO
   GOTO TOP
   COPY TO A:\CLAS_PRO FOR CLAS_PRO = "    7"
   USE
   USE CAD_QUAD
   GOTO TOP
   COPY TO A:\CAD_QUAD FOR CLAS_PRO = "    7"
   USE
   USE CAD_APAR
   GOTO TOP
   COPY TO A:\CAD_APAR FOR CLAS_PRO = "    7"
   USE
   USE UNI_IMOB
   GOTO TOP
   COPY TO A:\UNI_IMOB FOR CLAS_PRO = "    7"
   USE
   USE CAD_VEND
   GOTO TOP
   COPY TO A:\CAD_VEND FOR CLAS_PRO = "    7"   
   USE
   USE CAD_QUIT 
   COPY TO A:\CAD_QUIT FOR CLAS_PRO = "    7"      
   USE
   USE INDICES
   GOTO TOP
   COPY TO A:\INDICES                              
   USE
   USE TAXA_MES                     
   GOTO TOP
   COPY TO A:\TAXA_MES                             
   USE
   USE CORRETOR 
   COPY TO A:\CORRETOR
   USE
   SELE 1
   USE CAD_QUAD INDEX CAD_QUAD
   SET FILTER TO CADASTRO 
   GOTO TOP
   DO WHILE .NOT. EOF()
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_LOTEAM))))+ALLTRIM(NUM_LOTEAM)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_QUADRA))))+ALLTRIM(NUM_QUADRA) 
      SELE 2
      USE &eARQ
      COPY TO A:\&eARQ
      USE
      SELE 1
      SKIP
   ENDDO
   SELE 1
   USE CAD_APAR INDEX CAD_APAR
   SET FILTER TO CADASTRO 
   GOTO TOP
   DO WHILE .NOT. EOF()
      eZERO = "000"
      eARQ  = "CP"+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_LOTEAM))))+ALLTRIM(NUM_LOTEAM)+SUBSTR(eZERO,1,(3-LEN(ALLTRIM(NUM_BLOCO))))+ALLTRIM(NUM_BLOCO) 
      SELE 2
      USE &eARQ
      COPY TO A:\&eARQ
      USE
      SELE 1
      SKIP
   ENDDO
   SELE 1
   USE
   SELE 2
   USE
   SELE 1
   USE
   SET EXCLUSIVE OFF 
CLOSE ALL
RETURN nil
********************* fim *********************************





