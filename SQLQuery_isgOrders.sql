--5/29/2018
--Attach rate for HDD

--check Details table
--Extract HDD_QTY from Details Table
SELECT Order_Date_week, SUM(ITM_QTY) AS HDD_QTY, SUM(SYS_QTY) AS SYS_QTY
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrdersDetails]
WHERE ITM_TYPE = 'HDD' OR ITM_TYPE LIKE 'HARD%DRIVE%' AND DELL_EMC_ORDER_FLAG = 'DELL' AND (LOB_DESC IN ('PowerEdge','Cloud Products') OR MGMT_PROD_LVL_3_NM = 'Storage')
GROUP BY Order_Date_week
ORDER BY Order_Date_week

--check isgOrders table
--There is no ITM_TYPE, so we cannot calculate SYS_QTY directly.

--Below SQL query for calculating HDD part quantity and associated system quantity by Region, Sales Business Unit, Customer, LOB, Platform, etc.
SELECT 
  C.Fiscal_Wk, 
  A.RGN_DESC, A.SLS_BU_LVL1_DESC,
       A.GBL_PARNT_ACCT_NM AS Customer, 
       CASE WHEN A.LOB_DESC = 'PowerEdge' AND A.ESI_order_flag = 'Y' THEN 'PowerEdge - ESI'
          WHEN A.MGMT_PROD_LVL_3_NM = 'Storage' AND A.DELL_EMC_ORDER_FLAG = 'DELL' THEN 'Storage - DELL'
       ELSE A.LOB_DESC END AS LOB_DESC, 
       A.BRAND_CATG_DESC, 
  SUM(B.PART_QTY) AS PART_QTY, SUM(A.SYS_QTY_DELL) AS SYS_QTY,  
  CASE WHEN SUM(A.SYS_QTY_DELL)>0 THEN SUM(B.PART_QTY)/SUM(A.SYS_QTY_DELL) END AS ATT_RATE 
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrders] A 
JOIN (
       SELECT ORD_NBR, LOB_DESC, BRAND_CATG_DESC, SUM(ITM_QTY) AS PART_QTY
       FROM IRIS.[Base].[ISG_Business_Transformation.isgOrdersDetails] 
       WHERE ITM_TYPE = 'HDD' OR ITM_TYPE LIKE 'HARD%DRIVE%' 
       GROUP BY ORD_NBR, LOB_DESC, BRAND_CATG_DESC
) AS B
ON A.ORD_NBR = B.ORD_NBR AND A.LOB_DESC = B.LOB_DESC AND A.BRAND_CATG_DESC = B.BRAND_CATG_DESC
JOIN IRIS.DIM.Date C
ON A.ORD_DT = C.Fiscal_Date
WHERE  
       C.Fiscal_Yr >= 'FY17' AND A.DELL_EMC_ORDER_FLAG = 'DELL' AND (A.LOB_DESC IN ('PowerEdge','Cloud Products') OR A.MGMT_PROD_LVL_3_NM = 'Storage')
GROUP BY 
  C.Fiscal_Wk,
  A.RGN_DESC, A.SLS_BU_LVL1_DESC,
       A.GBL_PARNT_ACCT_NM, 
       CASE WHEN A.LOB_DESC = 'PowerEdge' AND A.ESI_order_flag = 'Y' THEN 'PowerEdge - ESI'
            WHEN A.MGMT_PROD_LVL_3_NM = 'Storage' AND A.DELL_EMC_ORDER_FLAG = 'DELL' THEN 'Storage - DELL'
       ELSE A.LOB_DESC END,
       A.BRAND_CATG_DESC
ORDER BY C.Fiscal_Wk, BRAND_CATG_DESC

-- What is the table C?
SELECT TOP 100 *
FROM IRIS.DIM.Date