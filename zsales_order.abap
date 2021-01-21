// Ztable - Sales Order Table //


@EndUserText.label : 'Sales Order Master'
@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zsales_order {
  key client      : abap.clnt not null;
  key sales_order : abap.char(10) not null;
  customer        : abap.char(6);
  created_by      : abap.char(10);
  created_on      : abap.dats;
  @Semantics.amount.currencyCode : 'zsales_order.currency_code'
  total_sales     : /dmo/total_price;
  currency_code   : /dmo/currency_code;

}
