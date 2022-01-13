module Sfdc.BulkApiSupport
  ( BulkApiSupport
  )
where

-- # Bulk API Support
--
-- From <https://help.salesforce.com/articleView?id=000318512&language=en_US&type=1&mode=1>:
--
-- Generally speaking an sObject is supported in the Bulk API if it is available
-- in the SOAP API and has a key prefix or is AccountPartner/OpportunityPartner.
-- So this list can be obtained by checking the information returned by a
-- describeGlobal() call. Also, to perform a Bulk API query, the sObject must be
-- queryable, which can be checked via a describeSObject() call or by checking
-- the list of "Supported Calls" in the sObject's SOAP API documentation page
-- (e.g.
-- https://developer.salesforce.com/docs/atlas.en-us.api.meta/api/sforce_api_objects_account.htm).
--
-- Below is a non-comprehensive list of sObjects that are not currently (as of
-- Summer '18) available in the Bulk API:
--   - *Feed (e.g. AccountFeed, AssetFeed, ...)
--   - *Share (e.g. AccountBrandShare, ChannelProgramLevelShare, ...)
--   - *History (e.g. AccountHistory, ActivityHistory, ...)
--   - *EventRelation (e.g. AcceptedEventRelation, DeclinedEventRelation, ...)
--   - AggregateResult
--   - AttachedContentDocument
--   - CaseStatus
--   - CaseTeamMember
--   - CaseTeamRole
--   - CaseTeamTemplate
--   - CaseTeamTemplateMember
--   - CaseTeamTemplateRecord
--   - CombinedAttachment
--   - ContentFolderItem
--   - ContractStatus
--   - EventWhoRelation
--   - FolderedContentDocument
--   - KnowledgeArticleViewStat
--   - KnowledgeArticleVoteStat
--   - LookedUpFromActivity
--   - Name
--   - NoteAndAttachment
--   - OpenActivity
--   - OwnedContentDocument
--   - PartnerRole
--   - RecentlyViewed
--   - ServiceAppointmentStatus
--   - SolutionStatus
--   - TaskPriority
--   - TaskStatus
--   - TaskWhoRelation
--   - UserRecordAccess
--   - WorkOrderLineItemStatus
--   - WorkOrderStatus

-- # Entity key prefixes
--
-- From <https://help.salesforce.com/articleView?id=000325244&language=en_US&type=1&mode=1>:
--
-- The entity type of every Salesforce record ID is encoded in its first three
-- characters. This table can be used to look up the entity name. For example,
-- given ID 00D0000000000XX, if you look up "00D" in the table below, you'll
-- find that the entity is Organization.
--
-- [Table omitted here]

class BulkApiSupport a
