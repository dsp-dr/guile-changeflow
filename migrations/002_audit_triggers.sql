-- Comprehensive Audit Trigger System for ITIL Compliance
-- Version: 002
-- Date: 2025-09-13
-- Purpose: Create audit triggers for all tables to ensure complete traceability

-- Function to create JSON from record values
-- SQLite doesn't have native JSON_OBJECT until 3.33+, so we build it manually

-- Audit trigger for change_requests table
CREATE TRIGGER IF NOT EXISTS audit_change_requests_insert
AFTER INSERT ON change_requests
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, new_values)
    VALUES (
        'change_requests',
        NEW.id,
        'INSERT',
        json_object(
            'id', NEW.id,
            'title', NEW.title,
            'description', NEW.description,
            'risk_score', NEW.risk_score,
            'status', NEW.status,
            'priority', NEW.priority,
            'category', NEW.category,
            'impact', NEW.impact,
            'urgency', NEW.urgency,
            'requestor_id', NEW.requestor_id,
            'assignee_id', NEW.assignee_id,
            'created_at', NEW.created_at
        )
    );
END;

CREATE TRIGGER IF NOT EXISTS audit_change_requests_update
AFTER UPDATE ON change_requests
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, old_values, new_values)
    VALUES (
        'change_requests',
        NEW.id,
        'UPDATE',
        json_object(
            'title', OLD.title,
            'description', OLD.description,
            'risk_score', OLD.risk_score,
            'status', OLD.status,
            'priority', OLD.priority,
            'assignee_id', OLD.assignee_id,
            'approver_id', OLD.approver_id,
            'implementer_id', OLD.implementer_id
        ),
        json_object(
            'title', NEW.title,
            'description', NEW.description,
            'risk_score', NEW.risk_score,
            'status', NEW.status,
            'priority', NEW.priority,
            'assignee_id', NEW.assignee_id,
            'approver_id', NEW.approver_id,
            'implementer_id', NEW.implementer_id
        )
    );
END;

CREATE TRIGGER IF NOT EXISTS audit_change_requests_delete
AFTER DELETE ON change_requests
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, old_values)
    VALUES (
        'change_requests',
        OLD.id,
        'DELETE',
        json_object(
            'id', OLD.id,
            'title', OLD.title,
            'status', OLD.status,
            'risk_score', OLD.risk_score
        )
    );
END;

-- Audit triggers for users table
CREATE TRIGGER IF NOT EXISTS audit_users_insert
AFTER INSERT ON users
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, new_values)
    VALUES (
        'users',
        NEW.id,
        'INSERT',
        json_object(
            'id', NEW.id,
            'username', NEW.username,
            'email', NEW.email,
            'full_name', NEW.full_name,
            'role', NEW.role,
            'department', NEW.department,
            'active', NEW.active
        )
    );
END;

CREATE TRIGGER IF NOT EXISTS audit_users_update
AFTER UPDATE ON users
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, old_values, new_values)
    VALUES (
        'users',
        NEW.id,
        'UPDATE',
        json_object(
            'username', OLD.username,
            'email', OLD.email,
            'full_name', OLD.full_name,
            'role', OLD.role,
            'active', OLD.active
        ),
        json_object(
            'username', NEW.username,
            'email', NEW.email,
            'full_name', NEW.full_name,
            'role', NEW.role,
            'active', NEW.active
        )
    );
END;

-- Audit triggers for approvals table
CREATE TRIGGER IF NOT EXISTS audit_approvals_insert
AFTER INSERT ON approvals
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, new_values)
    VALUES (
        'approvals',
        CAST(NEW.id AS TEXT),
        'INSERT',
        json_object(
            'change_request_id', NEW.change_request_id,
            'approver_id', NEW.approver_id,
            'decision', NEW.decision,
            'comments', NEW.comments,
            'conditions', NEW.conditions
        )
    );
END;

CREATE TRIGGER IF NOT EXISTS audit_approvals_update
AFTER UPDATE ON approvals
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, old_values, new_values)
    VALUES (
        'approvals',
        CAST(NEW.id AS TEXT),
        'UPDATE',
        json_object(
            'decision', OLD.decision,
            'comments', OLD.comments,
            'conditions', OLD.conditions
        ),
        json_object(
            'decision', NEW.decision,
            'comments', NEW.comments,
            'conditions', NEW.conditions
        )
    );
END;

-- Special trigger for state transitions - log the transition itself
CREATE TRIGGER IF NOT EXISTS audit_state_transition
AFTER INSERT ON state_transitions
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, new_values, user_id)
    VALUES (
        'state_transitions',
        NEW.change_request_id,
        'INSERT',
        json_object(
            'from_state', NEW.from_state,
            'to_state', NEW.to_state,
            'reason', NEW.reason,
            'transitioned_by', NEW.transitioned_by
        ),
        NEW.transitioned_by
    );
END;

-- Audit triggers for risk_assessments
CREATE TRIGGER IF NOT EXISTS audit_risk_assessments_insert
AFTER INSERT ON risk_assessments
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, new_values, user_id)
    VALUES (
        'risk_assessments',
        CAST(NEW.id AS TEXT),
        'INSERT',
        json_object(
            'change_request_id', NEW.change_request_id,
            'risk_score', NEW.risk_score,
            'probability', NEW.probability,
            'impact_level', NEW.impact_level,
            'assessed_by', NEW.assessed_by
        ),
        NEW.assessed_by
    );
END;

-- Audit triggers for comments
CREATE TRIGGER IF NOT EXISTS audit_comments_insert
AFTER INSERT ON comments
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, new_values, user_id)
    VALUES (
        'comments',
        CAST(NEW.id AS TEXT),
        'INSERT',
        json_object(
            'change_request_id', NEW.change_request_id,
            'comment_text', NEW.comment_text,
            'comment_type', NEW.comment_type,
            'is_internal', NEW.is_internal
        ),
        NEW.user_id
    );
END;

CREATE TRIGGER IF NOT EXISTS audit_comments_update
AFTER UPDATE ON comments
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, old_values, new_values, user_id)
    VALUES (
        'comments',
        CAST(NEW.id AS TEXT),
        'UPDATE',
        json_object('comment_text', OLD.comment_text),
        json_object('comment_text', NEW.comment_text),
        NEW.user_id
    );
END;

-- Audit triggers for attachments
CREATE TRIGGER IF NOT EXISTS audit_attachments_insert
AFTER INSERT ON attachments
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, new_values, user_id)
    VALUES (
        'attachments',
        CAST(NEW.id AS TEXT),
        'INSERT',
        json_object(
            'change_request_id', NEW.change_request_id,
            'filename', NEW.filename,
            'file_size', NEW.file_size,
            'mime_type', NEW.mime_type
        ),
        NEW.uploaded_by
    );
END;

CREATE TRIGGER IF NOT EXISTS audit_attachments_delete
AFTER DELETE ON attachments
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, old_values)
    VALUES (
        'attachments',
        CAST(OLD.id AS TEXT),
        'DELETE',
        json_object(
            'change_request_id', OLD.change_request_id,
            'filename', OLD.filename
        )
    );
END;

-- Audit triggers for dependencies
CREATE TRIGGER IF NOT EXISTS audit_dependencies_insert
AFTER INSERT ON dependencies
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, new_values)
    VALUES (
        'dependencies',
        CAST(NEW.id AS TEXT),
        'INSERT',
        json_object(
            'change_request_id', NEW.change_request_id,
            'depends_on_id', NEW.depends_on_id,
            'dependency_type', NEW.dependency_type
        )
    );
END;

-- Audit triggers for affected_systems
CREATE TRIGGER IF NOT EXISTS audit_affected_systems_insert
AFTER INSERT ON affected_systems
BEGIN
    INSERT INTO audit_log (table_name, record_id, action, new_values)
    VALUES (
        'affected_systems',
        CAST(NEW.id AS TEXT),
        'INSERT',
        json_object(
            'change_request_id', NEW.change_request_id,
            'system_name', NEW.system_name,
            'system_type', NEW.system_type,
            'downtime_required', NEW.downtime_required
        )
    );
END;

-- Compliance validation trigger - ensure critical changes have approvals
CREATE TRIGGER IF NOT EXISTS validate_critical_change_approval
BEFORE UPDATE ON change_requests
WHEN NEW.status = 'approved' AND NEW.priority = 'critical'
BEGIN
    SELECT CASE
        WHEN (SELECT COUNT(*) FROM approvals
              WHERE change_request_id = NEW.id
              AND decision = 'approved') < 2
        THEN RAISE(ABORT, 'Critical changes require at least 2 approvals')
    END;
END;

-- Trigger to prevent status regression (ITIL process compliance)
CREATE TRIGGER IF NOT EXISTS prevent_status_regression
BEFORE UPDATE ON change_requests
WHEN OLD.status IN ('completed', 'cancelled', 'rejected')
BEGIN
    SELECT CASE
        WHEN NEW.status != OLD.status
        THEN RAISE(ABORT, 'Cannot change status from terminal state')
    END;
END;

-- Trigger to auto-create notification on high-risk changes
CREATE TRIGGER IF NOT EXISTS notify_high_risk_changes
AFTER INSERT ON change_requests
WHEN NEW.risk_score >= 75
BEGIN
    INSERT INTO notifications (user_id, change_request_id, notification_type, title, message)
    SELECT DISTINCT u.id, NEW.id, 'risk_update',
           'High Risk Change Created',
           'Change request ' || NEW.id || ' has been created with risk score ' || NEW.risk_score
    FROM users u
    WHERE u.role IN ('admin', 'cab_member');
END;

-- Trigger to track approval completion
CREATE TRIGGER IF NOT EXISTS auto_approve_when_threshold_met
AFTER INSERT ON approvals
WHEN NEW.decision = 'approved'
BEGIN
    UPDATE change_requests
    SET status = 'approved',
        approver_id = NEW.approver_id
    WHERE id = NEW.change_request_id
    AND status = 'assessing'
    AND (
        SELECT COUNT(*)
        FROM approvals
        WHERE change_request_id = NEW.change_request_id
        AND decision = 'approved'
    ) >= CASE
        WHEN (SELECT priority FROM change_requests WHERE id = NEW.change_request_id) = 'critical' THEN 2
        ELSE 1
    END;
END;