#!/usr/bin/env python

"""
Simple example showing node and relationship creation plus
execution of Cypher queries
"""

from __future__ import print_function

# Import Neo4j modules
from py2neo import neo4j, cypher

# Attach to the graph db instance
graph_db = neo4j.GraphDatabaseService("http://localhost:7474/db/data/")

# Create two nodes
node_a, node_b = graph_db.create(
    {"name": "Alice"},
    {"name": "Bob"}
)

node_a_alias = graph_db.create(
    {"name": "Alice in Wonderland"},
    {"name": "Little girl Alice"}
)

# Join the nodes with a relationship
node_a.create_relationship_to(node_b, "KNOWS")
node_a.create_relationship_to(node_a_alias[0], "ALSO_KNOWN_AS")
node_a.create_relationship_to(node_a_alias[1], "ALSO_KNOWN_AS")

# Build a Cypher query
query = "START a=node({A}) MATCH a-[:KNOWS]->b RETURN a,b"

# Define a row handler...
def print_row(row):
    a, b = row
    print(a["name"] + " knows " + b["name"])

# ...and execute the query
cypher.execute(graph_db, query, {"A": node_a.id}, row_handler=print_row)
