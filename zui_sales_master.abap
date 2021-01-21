// Overwriting UI Annotations on CDS View // 

@AbapCatalog.sqlViewName: 'ZVIEW_SALES'
@AbapCatalog.compiler.compareFilter: false
@AbapCatalog.preserveKey
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales & CUstomer'

@UI: {
 headerInfo: { typeName: 'SalesM', typeNamePlural: 'SalesOrders', title: { type: #STANDARD, value: 'SalesOrder' } } }

@Search.searchable: true
define root view zc_sales_master
  as select from zsales_master
  association [0..1] to /DMO/I_Customer as _customer on $projection.customer = _customer.CustomerID
  association [0..1] to I_Currency      as _Currency on $projection.currency_code = _Currency.Currency

{
 @UI.facet: [ { id:              'Sales',
                purpose:         #STANDARD,
                type:            #IDENTIFICATION_REFERENCE,
                label:           'Sales',
                position:        10 } 
                ]


                 
      @UI: {
          lineItem:       [ { label: 'Sales Order', position: 10, importance: #HIGH } ] }
      @Search.defaultSearchElement: true
  key sales_order as SalesOrder,

      @UI: {
      lineItem:       [ { position: 20, importance: #HIGH } ],
      selectionField: [ { position: 20 } ] }

      @Consumption.valueHelpDefinition:[ { entity: { name: '/DMO/I_Customer', element: 'customer_id'  } } ]
      customer,

      @UI: {
      lineItem:       [ { position: 30, importance: #HIGH } ],
      selectionField: [ { position: 30 } ] }
      created_by,

      @UI: {
      lineItem:       [ { position: 40, importance: #HIGH } ] }
      created_on,

      @UI: {
      lineItem:       [ { position: 50, importance: #HIGH } ] ,
      identification: [{ position: 50 }] }

      @Semantics.amount.currencyCode: 'currency_code'
      total_sales,


      @Semantics.currencyCode: true
      currency_code,
      @UI: {
      lineItem:       [ { position: 60, importance: #HIGH } ] ,
      identification: [{ position: 60 }] }

      _customer.City,
      //
      @UI: {
       lineItem:       [ { label: 'Customer Name', position: 70, importance: #HIGH } ] ,
       identification: [{ label: 'Customer Name', position: 70 }] }

      concat( _customer.FirstName , _customer.LastName ) as customer_name,
      //
      _customer, // Make association public
      _Currency
}
