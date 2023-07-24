@AbapCatalog.sqlViewName: '/YKY/IEXPOATTALV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Export Attachment CDS for ALV'
define view /YKY/EXPORT_ATT_ALV as select from /yky/exports_att
{
    key attachment_uuid as AttachmentUUID,
    export_uuid as ExportUUID,
    artifact_id as ArtifactID,
    expense_id as ExpenseID,
    filename as Filename,
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
