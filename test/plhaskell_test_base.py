from unittest import TestCase
from psycopg import connect
from psycopg.rows import dict_row


class PLHaskellTestBase(TestCase):
    def get_notice(self, diag):
        self.notices.append((diag.severity, diag.message_primary))

    def setUp(self):
        self.conn = connect(row_factory=dict_row)

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA IF EXISTS plhaskell_test CASCADE")
            cur.execute("CREATE SCHEMA plhaskell_test")
            cur.execute("SET search_path TO plhaskell_test")

            self.type_setup(cur)

        self.conn.commit()

        self.notices = []
        self.conn.add_notice_handler(self.get_notice)

    def tearDown(self):
        if self.conn.closed:
            self.conn = connect(row_factory=dict_row)

        self.conn.rollback()

        with self.conn.cursor() as cur:
            cur.execute("DROP SCHEMA plhaskell_test CASCADE")

        self.conn.commit()

        self.conn.close()

    @staticmethod
    def type_setup(_cur):
        pass

    def execute_file(self, file):
        with self.conn.cursor() as cur:
            with open(file, "rt") as file:
                cur.execute(file.read())
