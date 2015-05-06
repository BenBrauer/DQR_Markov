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
    data clob,
    dataset_id integer,
    FOREIGN KEY(dataset_id) REFERENCES dataset(id)
    
);

 
# --- !Downs
 
DROP TABLE dataset;
DROP SEQUENCE dastset_id_seq;

DROP TABLE relation;
DROP SEQUENCE relation_id_seq;