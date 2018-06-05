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

--05/30/2018
--Below SQL query for calculating HDD part quantity and associated system quantity by Region, Sales Business Unit, Customer, LOB, Platform, etc.
SELECT 
  C.Fiscal_Wk,  C.Fiscal_Wk_End_Date, C.Fiscal_Date, C.Fiscal_Day, C.Fiscal_Mo, C.Fiscal_Mo_End_Date, C.Fiscal_Yr, C.Fiscal_Qtr,
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
       C.Fiscal_Yr >= 'FY16' AND A.DELL_EMC_ORDER_FLAG = 'DELL' AND (A.LOB_DESC IN ('PowerEdge','Cloud Products') OR A.MGMT_PROD_LVL_3_NM = 'Storage')
GROUP BY 
  C.Fiscal_Wk,  C.Fiscal_Wk_End_Date, C.Fiscal_Date, C.Fiscal_Day, C.Fiscal_Mo, C.Fiscal_Mo_End_Date,  C.Fiscal_Yr, C.Fiscal_Qtr,
  A.RGN_DESC, A.SLS_BU_LVL1_DESC,
       A.GBL_PARNT_ACCT_NM, 
       CASE WHEN A.LOB_DESC = 'PowerEdge' AND A.ESI_order_flag = 'Y' THEN 'PowerEdge - ESI'
            WHEN A.MGMT_PROD_LVL_3_NM = 'Storage' AND A.DELL_EMC_ORDER_FLAG = 'DELL' THEN 'Storage - DELL'
       ELSE A.LOB_DESC END,
       A.BRAND_CATG_DESC
ORDER BY C.Fiscal_Wk, BRAND_CATG_DESC

SELECT TOP 100 *
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrders]

--no CFG information?
--Got a quenry from Cinkie
SELECT 
  D.Cfg_Desc AS CFG, C.Fiscal_Qtr, C.Fiscal_Mo, C.Fiscal_Wk, C.Fiscal_Wk_End_Date, C.Fiscal_Date,
  CASE WHEN A.LOB_DESC = 'PowerEdge' AND A.ESI_order_flag = 'Y' THEN 'PowerEdge - ESI'
                     WHEN A.MGMT_PROD_LVL_3_NM = 'Storage' AND A.DELL_EMC_ORDER_FLAG = 'DELL' THEN 'Storage - DELL'
                ELSE A.LOB_DESC END AS LOB_DESC, A.BRAND_CATG_DESC, 
  A.RGN_DESC,
  A.GBL_PARNT_ACCT_NM AS Customer, 
  SUM(SYS_QTY_DELL) AS SYS_QTY,
  SUM(ITM_QTY) AS PART_QTY
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrders] AS A 
JOIN IRIS.[Base].[ISG_Business_Transformation.isgOrdersDetails] B
ON A.ORD_NBR = B.ORD_NBR AND A.FMLY_PFOLIO_DESC = B.FMLY_PFOLIO_DESC AND A.SRC_BU_ID = B.SRC_BU_ID
JOIN IRIS.DIM.Date C
ON B.ORD_DT = C.Fiscal_Date
JOIN IRIS_Data_Mart.[dbo].[SKU_CFG_Bridge] D
ON B.ITM_NBR = D.sku_num
WHERE D.Cfg_Desc LIKE '%HDD%' AND Commodity_Desc IN ('Hard Drive', 'Controller Cards/HBA') AND C.Fiscal_Yr >= 'FY16' AND A.DELL_EMC_ORDER_FLAG = 'DELL' AND (A.LOB_DESC IN ('PowerEdge','Cloud Products') OR A.MGMT_PROD_LVL_3_NM = 'Storage')
GROUP BY
  D.Cfg_Desc, C.Fiscal_Qtr, C.Fiscal_Mo, C.Fiscal_Wk, C.Fiscal_Wk_End_Date, C.Fiscal_Date,
  CASE WHEN A.LOB_DESC = 'PowerEdge' AND A.ESI_order_flag = 'Y' THEN 'PowerEdge - ESI'
                   WHEN A.MGMT_PROD_LVL_3_NM = 'Storage' AND A.DELL_EMC_ORDER_FLAG = 'DELL' THEN 'Storage - DELL'
                   ELSE A.LOB_DESC END, A.BRAND_CATG_DESC, 
  A.RGN_DESC, A.GBL_PARNT_ACCT_NM
ORDER BY D.Cfg_Desc, C.Fiscal_Qtr, C.Fiscal_Mo, C.Fiscal_Wk

--5/31/2018
SELECT 
  D.Cfg_Desc AS CFG, C.Fiscal_Qtr, C.Fiscal_Mo, C.Fiscal_Wk, C.Fiscal_Wk_End_Date, C.Fiscal_Date, C.Fiscal_Yr, B.Order_Date_week, C.Fiscal_Mo_End_Date,  C.Fiscal_Qtr_End_Date,
  CASE WHEN A.LOB_DESC = 'PowerEdge' AND A.ESI_order_flag = 'Y' THEN 'PowerEdge - ESI'
                     WHEN A.MGMT_PROD_LVL_3_NM = 'Storage' AND A.DELL_EMC_ORDER_FLAG = 'DELL' THEN 'Storage - DELL'
                ELSE A.LOB_DESC END AS LOB_DESC, A.BRAND_CATG_DESC, 
  A.RGN_DESC,
  A.GBL_PARNT_ACCT_NM AS Customer, 
  SUM(SYS_QTY_DELL) AS SYS_QTY,
  SUM(ITM_QTY) AS PART_QTY
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrders] AS A 
JOIN IRIS.[Base].[ISG_Business_Transformation.isgOrdersDetails] B
ON A.ORD_NBR = B.ORD_NBR AND A.FMLY_PFOLIO_DESC = B.FMLY_PFOLIO_DESC AND A.SRC_BU_ID = B.SRC_BU_ID
JOIN IRIS.DIM.Date C
ON B.ORD_DT = C.Fiscal_Date
JOIN IRIS_Data_Mart.[dbo].[SKU_CFG_Bridge] D
ON B.ITM_NBR = D.sku_num
WHERE D.Cfg_Desc LIKE '%HDD%' AND Commodity_Desc IN ('Hard Drive', 'Controller Cards/HBA') AND C.Fiscal_Yr >= 'FY16' AND A.DELL_EMC_ORDER_FLAG = 'DELL' AND (A.LOB_DESC IN ('PowerEdge','Cloud Products') OR A.MGMT_PROD_LVL_3_NM = 'Storage')
GROUP BY
  D.Cfg_Desc, C.Fiscal_Qtr, C.Fiscal_Mo, C.Fiscal_Wk, C.Fiscal_Wk_End_Date, C.Fiscal_Date, C.Fiscal_Yr, B.Order_Date_week, C.Fiscal_Mo_End_Date,  C.Fiscal_Qtr_End_Date,
  CASE WHEN A.LOB_DESC = 'PowerEdge' AND A.ESI_order_flag = 'Y' THEN 'PowerEdge - ESI'
                   WHEN A.MGMT_PROD_LVL_3_NM = 'Storage' AND A.DELL_EMC_ORDER_FLAG = 'DELL' THEN 'Storage - DELL'
                   ELSE A.LOB_DESC END, A.BRAND_CATG_DESC, 
  A.RGN_DESC, A.GBL_PARNT_ACCT_NM
ORDER BY D.Cfg_Desc, C.Fiscal_Qtr, C.Fiscal_Mo, C.Fiscal_Wk

--Check the weekly data