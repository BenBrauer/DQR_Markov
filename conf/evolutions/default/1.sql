# Tasks schema
 
# --- !Ups

CREATE SEQUENCE dataset_id_seq;
CREATE TABLE dataset (
    id integer NOT NULL DEFAULT nextval('dataset_id_seq'),
    label varchar(255)
);

INSERT INTO dataset (label) VALUES ('test1');
INSERT INTO dataset (label) VALUES ('test2');

CREATE SEQUENCE relation_id_seq;
CREATE TABLE relation (
    id integer NOT NULL DEFAULT nextval('relation_id_seq'),
    label varchar(255),
    data clob,
    dataset_id integer,
    FOREIGN KEY(dataset_id) REFERENCES dataset(id)
    
);

INSERT INTO relation (label, data, dataset_id) VALUES ('test1_r1',STRINGDECODE('column1,column2,column3\r\nhello,world,foo\r\nbar,this,is\r\na,csv,textr'),1);
INSERT INTO relation (label, data, dataset_id) VALUES ('test1_r2',STRINGDECODE('column1,column2,column3\r\nhello,world,foo\r\nbar,this,is\r\na,csv,textr'),1);
INSERT INTO relation (label, data, dataset_id) VALUES ('test1_r3',STRINGDECODE('column1,column2,column3\r\nhello,world,foo\r\nbar,this,is\r\na,csv,textr'),1);
INSERT INTO relation (label, data, dataset_id) VALUES ('test2_r1',STRINGDECODE('column1,column2,column3\r\nhello,world,foo\r\nbar,this,is\r\na,csv,textr'),2);
INSERT INTO relation (label, data, dataset_id) VALUES ('test2_r2',STRINGDECODE('column1,column2,column3\r\nhello,world,foo\r\nbar,this,is\r\na,csv,textr')	,2); 

CREATE SEQUENCE rule_id_seq;
CREATE TABLE rule (
	id integer NOT NULL DEFAULT nextval('rule_id_seq'),
	label varchar(255),
	rule varchar(500),
	dataset_id integer,
	FOREIGN KEY (dataset_id) REFERENCES dataset(id)
);

INSERT INTO rule (label, rule, dataset_id) VALUES ('rule1_r1', 'TEST',1);
INSERT INTO rule (label, rule, dataset_id) VALUES ('rule2_r1', 'TEST',1);
INSERT INTO rule (label, rule, dataset_id) VALUES ('rule1_r2', 'TEST',2);
INSERT INTO rule (label, rule, dataset_id) VALUES ('rule2_r2', 'TEST',2);

# --- !Downs
 
DROP TABLE dataset;
DROP SEQUENCE dataset_id_seq;

DROP TABLE relation;
DROP SEQUENCE relation_id_seq;

DROP TABLE rule;
DROP SEQUENCE rule_id_seq;