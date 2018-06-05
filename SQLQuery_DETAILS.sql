--5/23/2018
-- Show TOP 100 Order Numbers
SELECT TOP 100 *
FROM [IRIS].[Base].[ISG_Business_Transformation.isgOrdersDetails]

-- What item types are there?
SELECT DISTINCT ITM_TYPE
FROM [IRIS].[Base].[ISG_Business_Transformation.isgOrdersDetails]
ORDER BY ITM_TYPE

--Maybe HDD?
SELECT DISTINCT ITM_TYPE, ITM_DESC, PROD_GRP_DESC, LOB_DESC
FROM [IRIS].[Base].[ISG_Business_Transformation.isgOrdersDetails]
WHERE ITM_TYPE IN ('HDD','Hard Drive - First Drive','HARDDRIVE','HDD Configuration','HDD Filler','HDD Info','HDD Options')
ORDER BY ITM_TYPE
--Could not decide yet.

--Join Talbe details and orders
--Only 'HDD'
SELECT TOP 10 *
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrders] A 
JOIN IRIS.[Base].[ISG_Business_Transformation.isgOrdersDetails] B
ON A.ORD_NBR = B.ORD_NBR
WHERE ITM_TYPE = 'HDD'

--HDD?
SELECT TOP 10 *
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrders] A 
JOIN IRIS.[Base].[ISG_Business_Transformation.isgOrdersDetails] B
ON A.ORD_NBR = B.ORD_NBR
WHERE ITM_TYPE = 'HARDDRIVE'
--Compare these results in Excel

--All HDD
SELECT DISTINCT ITM_TYPE, ITM_DESC, PROD_GRP_DESC, LOB_DESC, MGMT_PROD_LVL_3_NM
FROM [IRIS].[Base].[ISG_Business_Transformation.isgOrdersDetails]
WHERE ITM_TYPE IN ('HDD','Hard Drive - First Drive','HARDDRIVE')
ORDER BY ITM_TYPE, LOB_DESC, MGMT_PROD_LVL_3_NM
--Confirmed with Cinkie
--Now I see why I should use the following filters.

--From Cinkie
SELECT ITM_TYPE, ITM_DESC, SUM(ITM_QTY) AS ITM_QTY
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrdersDetails] A
WHERE ITM_TYPE = 'HDD' OR ITM_TYPE LIKE 'HARD%DRIVE%' AND ORD_DT >= '2017-01-01' AND A.DELL_EMC_ORDER_FLAG = 'DELL' AND (A.LOB_DESC IN ('PowerEdge','Cloud Products') OR A.MGMT_PROD_LVL_3_NM = 'Storage')
GROUP BY ITM_TYPE, ITM_DESC
ORDER BY ITM_TYPE, ITM_DESC
--Code from Cinkie Ends

--Extract data needed
--Extract HDD_QTY from Details Table
SELECT Order_Date_week, SUM(ITM_QTY) AS HDD_QTY
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrdersDetails]
WHERE ITM_TYPE = 'HDD' OR ITM_TYPE LIKE 'HARD%DRIVE%' AND DELL_EMC_ORDER_FLAG = 'DELL' AND (LOB_DESC IN ('PowerEdge','Cloud Products') OR MGMT_PROD_LVL_3_NM = 'Storage')
GROUP BY Order_Date_week
ORDER BY Order_Date_week

--Extract HDD_QTY from 2 Tables
SELECT A.Order_Date_week, SUM(B.ITM_QTY) AS HDD_QTY
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrders] A 
JOIN IRIS.[Base].[ISG_Business_Transformation.isgOrdersDetails] B
ON A.ORD_NBR = B.ORD_NBR
WHERE  ITM_TYPE = 'HDD' OR ITM_TYPE LIKE 'HARD%DRIVE%' AND A.DELL_EMC_ORDER_FLAG = 'DELL' AND (A.LOB_DESC IN ('PowerEdge','Cloud Products') OR A.MGMT_PROD_LVL_3_NM = 'Storage')
GROUP BY A.Order_Date_week
ORDER BY A.Order_Date_week
--The same orders might have been couted repeatedly.
--The same orders are in the isgOrders because they are cancelled.

--Below SQL query for calculating HDD part quantity and associated system quantity by Region, Sales Business Unit, Customer, LOB, Platform, etc.
SELECT 
  C.Fiscal_Wk, 
  A.RGN_DESC, A.SLS_BU_LVL1_DESC,
       A.GBL_PARNT_ACCT_NM AS Customer, 
       CASE WHEN A.LOB_DESC = 'PowerEdge' AND A.ESI_order_flag = 'Y' THEN 'PowerEdge - ESI'
          WHEN A.MGMT_PROD_LVL_3_NM = 'Storage' AND A.DELL_EMC_ORDER_FLAG = 'DELL' THEN 'Storage - DELL'
       ELSE A.LOB_DESC END AS LOB_DESC, 
       A.BRAND_CATG_DESC, 
  SUM(B.PART_QTY) AS PART_QTY, SUM(A.SYS_QTY_DELL) AS SYS_QTY
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrders] A 
JOIN (
       SELECT ORD_NBR, LOB_DESC, BRAND_CATG_DESC, SUM(ITM_QTY) AS PART_QTY
       FROM IRIS.[Base].[ISG_Business_Transformation.isgOrdersDetails] 
       WHERE ITM_TYPE = 'HDD'
       GROUP BY ORD_NBR, LOB_DESC, BRAND_CATG_DESC
) AS B
ON A.ORD_NBR = B.ORD_NBR AND A.LOB_DESC = B.LOB_DESC AND A.BRAND_CATG_DESC = B.BRAND_CATG_DESC
JOIN IRIS.DIM.Date C
ON A.ORD_DT = C.Fiscal_Date
WHERE  
       C.Fiscal_Yr >= 'FY18' AND A.DELL_EMC_ORDER_FLAG = 'DELL' AND (A.LOB_DESC IN ('PowerEdge','Cloud Products') OR A.MGMT_PROD_LVL_3_NM = 'Storage')
GROUP BY 
  C.Fiscal_Wk,
  A.RGN_DESC, A.SLS_BU_LVL1_DESC,
       A.GBL_PARNT_ACCT_NM, 
       CASE WHEN A.LOB_DESC = 'PowerEdge' AND A.ESI_order_flag = 'Y' THEN 'PowerEdge - ESI'
            WHEN A.MGMT_PROD_LVL_3_NM = 'Storage' AND A.DELL_EMC_ORDER_FLAG = 'DELL' THEN 'Storage - DELL'
       ELSE A.LOB_DESC END,
       A.BRAND_CATG_DESC
ORDER BY C.Fiscal_Wk


--05/24/2018
--Are there repeated order numbers?
SELECT TOP 10
  ORD_NBR, COUNT(*) AS ORD_REP
FROM
  IRIS.[Base].[ISG_Business_Transformation.isgOrders]
Where SYS_QTY_DELL>0 AND DELL_EMC_ORDER_FLAG = 'DELL' AND (LOB_DESC IN ('PowerEdge','Cloud Products') OR MGMT_PROD_LVL_3_NM = 'Storage')
GROUP BY ORD_NBR
ORDER BY ORD_REP DESC
--Wow, so many repeated orders! Now I see why.

--Check the order number that has so many repetitions
SELECT ORD_NBR, ORD_DT, TXN_DT, LOB_DESC, BRAND_CATG_DESC, GBL_PARNT_ACCT_NM, SYS_QTY_DELL, SYS_QTY_EMC
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrders]
WHERE ORD_NBR='310453687'
--It is from EMC, so remember to add these conditions.

--Check the order number that has so many repetitions
SELECT ORD_NBR, ORD_DT, TXN_DT, LOB_DESC, BRAND_CATG_DESC, GBL_PARNT_ACCT_NM, SYS_QTY_DELL, SYS_QTY_EMC
FROM IRIS.[Base].[ISG_Business_Transformation.isgOrders]
WHERE ORD_NBR='079947597'
--There are different POWEREDGE in this order.
--Thus, I think we can just calculate ITM_QTY uisng details table and SYS_QTY_DELL using isgOrders table.
--No, this does not work because we do not have the ITM type in isgOrders table.

--Select features that you want to plot
