@AbapCatalog.sqlViewName: '/YKY/IEXPORT_ALV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Export CDS for Alv'

define view /YKY/Export_ALV as select from /yky/exports
{
    key export_uuid as ExportUUID,
    expense_system as ExpenseSystem,
    export_id as ExportID,
    company_id as CompanyID,
    status as Status,
        
    @Semantics.user.createdBy: true
    created_by                      as CreatedBy,
    @Semantics.systemDateTime.createdAt: true
    created_at                      as CreatedAt,  
    @Semantics.user.lastChangedBy: true         
    changed_by                      as ChangedBy,
    @Semantics.systemDateTime.lastChangedAt: true
    changed_at                      as ChangedAt,
    
    deleted as Deleted
    
}
