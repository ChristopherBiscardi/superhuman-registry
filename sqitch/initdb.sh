#!/bin/bash
set -e

cd /opt/sqitch && sqitch -u "$POSTGRES_USER" deploy --verify
