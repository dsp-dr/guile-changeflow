-- ITIL-Compliant Change Management Database Schema
-- Version: 001
-- Date: 2025-09-13
-- Purpose: Production SQLite schema for Guile ChangeFlow with full audit capabilities

PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;

-- Core change_requests table (ITIL Change Management)
CREATE TABLE IF NOT EXISTS change_requests (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    description TEXT NOT NULL,
    risk_score INTEGER DEFAULT 0 CHECK(risk_score >= 0 AND risk_score <= 100),
    status TEXT NOT NULL DEFAULT 'submitted' CHECK(status IN (
        'submitted', 'assessing', 'approved', 'rejected',
        'implementing', 'completed', 'failed', 'cancelled', 'needs-info'
    )),
    priority TEXT DEFAULT 'medium' CHECK(priority IN ('low', 'medium', 'high', 'critical')),
    category TEXT DEFAULT 'standard' CHECK(category IN ('standard', 'normal', 'emergency', 'pre-approved')),
    impact TEXT DEFAULT 'low' CHECK(impact IN ('low', 'medium', 'high', 'critical')),
    urgency TEXT DEFAULT 'low' CHECK(urgency IN ('low', 'medium', 'high', 'critical')),

    -- Timestamps
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    scheduled_start TIMESTAMP,
    scheduled_end TIMESTAMP,
    actual_start TIMESTAMP,
    actual_end TIMESTAMP,

    -- Relationships
    requestor_id TEXT NOT NULL,
    assignee_id TEXT,
    approver_id TEXT,
    implementer_id TEXT,

    -- ITIL fields
    business_justification TEXT,
    implementation_plan TEXT,
    backout_plan TEXT,
    test_plan TEXT,
    communication_plan TEXT,

    -- Metadata
    external_id TEXT UNIQUE,
    source_system TEXT,
    tags TEXT, -- JSON array
    metadata TEXT -- JSON object
);

-- Users table (ITIL Service Desk)
CREATE TABLE IF NOT EXISTS users (
    id TEXT PRIMARY KEY,
    username TEXT UNIQUE NOT NULL,
    email TEXT UNIQUE NOT NULL,
    full_name TEXT NOT NULL,
    role TEXT DEFAULT 'user' CHECK(role IN ('user', 'approver', 'admin', 'implementer', 'cab_member')),
    department TEXT,
    active BOOLEAN DEFAULT 1,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Approvals table (ITIL Change Advisory Board)
CREATE TABLE IF NOT EXISTS approvals (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    change_request_id TEXT NOT NULL,
    approver_id TEXT NOT NULL,
    decision TEXT CHECK(decision IN ('approved', 'rejected', 'needs-info', 'abstain')),
    comments TEXT,
    conditions TEXT,
    approved_at TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (change_request_id) REFERENCES change_requests(id) ON DELETE CASCADE,
    FOREIGN KEY (approver_id) REFERENCES users(id),
    UNIQUE(change_request_id, approver_id)
);

-- State transitions table (ITIL Change Management Process)
CREATE TABLE IF NOT EXISTS state_transitions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    change_request_id TEXT NOT NULL,
    from_state TEXT,
    to_state TEXT NOT NULL,
    transitioned_by TEXT NOT NULL,
    reason TEXT,
    transition_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (change_request_id) REFERENCES change_requests(id) ON DELETE CASCADE,
    FOREIGN KEY (transitioned_by) REFERENCES users(id)
);

-- Audit log table (ITIL Audit Trail)
CREATE TABLE IF NOT EXISTS audit_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    table_name TEXT NOT NULL,
    record_id TEXT NOT NULL,
    action TEXT NOT NULL CHECK(action IN ('INSERT', 'UPDATE', 'DELETE')),
    user_id TEXT,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    old_values TEXT, -- JSON
    new_values TEXT, -- JSON
    ip_address TEXT,
    user_agent TEXT,
    session_id TEXT
);

-- Risk assessments table (ITIL Risk Management)
CREATE TABLE IF NOT EXISTS risk_assessments (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    change_request_id TEXT NOT NULL,
    assessed_by TEXT NOT NULL,
    risk_score INTEGER NOT NULL CHECK(risk_score >= 0 AND risk_score <= 100),
    probability TEXT CHECK(probability IN ('very-low', 'low', 'medium', 'high', 'very-high')),
    impact_level TEXT CHECK(impact_level IN ('minimal', 'low', 'moderate', 'high', 'severe')),
    risk_factors TEXT, -- JSON array
    mitigation_strategies TEXT, -- JSON array
    assessment_notes TEXT,
    assessed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (change_request_id) REFERENCES change_requests(id) ON DELETE CASCADE,
    FOREIGN KEY (assessed_by) REFERENCES users(id)
);

-- Comments table (ITIL Communication)
CREATE TABLE IF NOT EXISTS comments (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    change_request_id TEXT NOT NULL,
    user_id TEXT NOT NULL,
    comment_text TEXT NOT NULL,
    comment_type TEXT DEFAULT 'general' CHECK(comment_type IN ('general', 'technical', 'approval', 'risk', 'implementation')),
    is_internal BOOLEAN DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (change_request_id) REFERENCES change_requests(id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Attachments table (ITIL Documentation)
CREATE TABLE IF NOT EXISTS attachments (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    change_request_id TEXT NOT NULL,
    uploaded_by TEXT NOT NULL,
    filename TEXT NOT NULL,
    file_size INTEGER,
    mime_type TEXT,
    storage_path TEXT,
    checksum TEXT,
    uploaded_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (change_request_id) REFERENCES change_requests(id) ON DELETE CASCADE,
    FOREIGN KEY (uploaded_by) REFERENCES users(id)
);

-- Dependencies table (ITIL Configuration Management)
CREATE TABLE IF NOT EXISTS dependencies (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    change_request_id TEXT NOT NULL,
    depends_on_id TEXT NOT NULL,
    dependency_type TEXT DEFAULT 'blocks' CHECK(dependency_type IN ('blocks', 'relates-to', 'conflicts-with', 'requires')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (change_request_id) REFERENCES change_requests(id) ON DELETE CASCADE,
    FOREIGN KEY (depends_on_id) REFERENCES change_requests(id) ON DELETE CASCADE,
    UNIQUE(change_request_id, depends_on_id)
);

-- Affected systems table (ITIL Service Asset and Configuration Management)
CREATE TABLE IF NOT EXISTS affected_systems (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    change_request_id TEXT NOT NULL,
    system_name TEXT NOT NULL,
    system_type TEXT CHECK(system_type IN ('application', 'infrastructure', 'database', 'network', 'security', 'other')),
    impact_description TEXT,
    downtime_required BOOLEAN DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (change_request_id) REFERENCES change_requests(id) ON DELETE CASCADE
);

-- Notifications table (ITIL Event Management)
CREATE TABLE IF NOT EXISTS notifications (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id TEXT NOT NULL,
    change_request_id TEXT,
    notification_type TEXT NOT NULL CHECK(notification_type IN ('state_change', 'approval_required', 'comment', 'risk_update', 'schedule_change')),
    title TEXT NOT NULL,
    message TEXT NOT NULL,
    is_read BOOLEAN DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    read_at TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (change_request_id) REFERENCES change_requests(id) ON DELETE CASCADE
);

-- Create indexes for performance
CREATE INDEX idx_change_requests_status ON change_requests(status);
CREATE INDEX idx_change_requests_requestor ON change_requests(requestor_id);
CREATE INDEX idx_change_requests_assignee ON change_requests(assignee_id);
CREATE INDEX idx_change_requests_created_at ON change_requests(created_at);
CREATE INDEX idx_change_requests_scheduled_start ON change_requests(scheduled_start);

CREATE INDEX idx_approvals_change_request ON approvals(change_request_id);
CREATE INDEX idx_approvals_approver ON approvals(approver_id);

CREATE INDEX idx_state_transitions_change ON state_transitions(change_request_id);
CREATE INDEX idx_state_transitions_time ON state_transitions(transition_time);

CREATE INDEX idx_audit_log_table_record ON audit_log(table_name, record_id);
CREATE INDEX idx_audit_log_timestamp ON audit_log(timestamp);

CREATE INDEX idx_comments_change_request ON comments(change_request_id);
CREATE INDEX idx_notifications_user ON notifications(user_id, is_read);

-- Create views for common queries
CREATE VIEW IF NOT EXISTS v_active_changes AS
SELECT
    cr.*,
    u_requestor.full_name as requestor_name,
    u_assignee.full_name as assignee_name,
    COUNT(DISTINCT c.id) as comment_count,
    COUNT(DISTINCT a.id) as attachment_count
FROM change_requests cr
LEFT JOIN users u_requestor ON cr.requestor_id = u_requestor.id
LEFT JOIN users u_assignee ON cr.assignee_id = u_assignee.id
LEFT JOIN comments c ON cr.id = c.change_request_id
LEFT JOIN attachments a ON cr.id = a.change_request_id
WHERE cr.status NOT IN ('completed', 'cancelled', 'rejected')
GROUP BY cr.id;

CREATE VIEW IF NOT EXISTS v_pending_approvals AS
SELECT
    cr.id,
    cr.title,
    cr.risk_score,
    cr.priority,
    cr.created_at,
    u.full_name as requestor_name,
    COUNT(DISTINCT a.id) as approval_count
FROM change_requests cr
LEFT JOIN users u ON cr.requestor_id = u.id
LEFT JOIN approvals a ON cr.id = a.change_request_id
WHERE cr.status = 'assessing'
GROUP BY cr.id;

-- Trigger for updated_at timestamps
CREATE TRIGGER update_change_requests_timestamp
AFTER UPDATE ON change_requests
BEGIN
    UPDATE change_requests SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

CREATE TRIGGER update_users_timestamp
AFTER UPDATE ON users
BEGIN
    UPDATE users SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;

CREATE TRIGGER update_comments_timestamp
AFTER UPDATE ON comments
BEGIN
    UPDATE comments SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
END;