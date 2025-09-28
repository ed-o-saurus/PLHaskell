-- This is a "procedural language" extension of PostgreSQL
-- allowing the execution of code in Haskell within SQL code.
--
-- Copyright (C) 2025 Edward F. Behn, Jr.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

DO $$
DECLARE
    r RECORD;

    null_array int[];
    array_empty int[];
    array_1d int[];
    array_2d int[];
    array_3d int[];
    array_4d int[];
    array_5d int[];
    array_6d int[];
    x int;
    a int;
BEGIN
    null_array = mk_array(NULL);
    array_empty = mk_array(0);
    array_1d = mk_array(1);
    array_2d = mk_array(2);
    array_3d = mk_array(3);
    array_4d = mk_array(4);
    array_5d = mk_array(5);
    array_6d = mk_array(6);

    IF null_array is not NULL THEN
        raise EXCEPTION 'null_array failed';
    END IF;

    IF array_empty != '{}'::int[] THEN
        raise EXCEPTION 'empty array failed';
    END IF;

    FOR idx0 IN 20 .. 21 LOOP
        x = idx0 - 20;
        a = array_1d[idx0];

        IF x%3 = 0 THEN
            IF a is not null THEN
                raise EXCEPTION '1-d array failed';
            END IF;
        ELSE
            IF a is null THEN
                raise EXCEPTION '1-d array failed';
            END IF;
            IF a != x THEN
                raise EXCEPTION '1-d array failed';
            END IF;
        END IF;
    END LOOP;

    FOR idx0 IN 20 .. 22 LOOP
        FOR idx1 IN 21 .. 22 LOOP
            x = 2*(idx0-20)+(idx1-21);
            a = array_2d[idx0][idx1];

            IF x%3 = 0 THEN
                IF a is not null THEN
                    raise EXCEPTION '2-d array failed';
                END IF;
            ELSE
                IF a is null THEN
                    raise EXCEPTION '2-d array failed';
                END IF;
                IF a != x THEN
                    raise EXCEPTION '2-d array failed';
                END IF;
            END IF;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 23 LOOP
        FOR idx1 IN 21 .. 23 LOOP
            FOR idx2 IN 22 .. 23 LOOP
                x = 2*(3*(idx0-20)+(idx1-21))+(idx2-22);
                a = array_3d[idx0][idx1][idx2];

                IF x%3 = 0 THEN
                    IF a is not null THEN
                        raise EXCEPTION '3-d array failed';
                    END IF;
                ELSE
                    IF a is null THEN
                        raise EXCEPTION '3-d array failed';
                    END IF;
                    IF a != x THEN
                        raise EXCEPTION '3-d array failed';
                    END IF;
                END IF;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 24 LOOP
        FOR idx1 IN 21 .. 24 LOOP
            FOR idx2 IN 22 .. 24 LOOP
                FOR idx3 IN 23 .. 24 LOOP
                    x = 2*(3*(4*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23);
                    a = array_4d[idx0][idx1][idx2][idx3];

                    IF x%3 = 0 THEN
                        IF a is not null THEN
                            raise EXCEPTION '4-d array failed';
                        END IF;
                    ELSE
                        IF a is null THEN
                            raise EXCEPTION '4-d array failed';
                        END IF;
                        IF a != x THEN
                            raise EXCEPTION '4-d array failed';
                        END IF;
                    END IF;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 25 LOOP
        FOR idx1 IN 21 .. 25 LOOP
            FOR idx2 IN 22 .. 25 LOOP
                FOR idx3 IN 23 .. 25 LOOP
                    FOR idx4 IN 24 .. 25 LOOP
                        x = 2*(3*(4*(5*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23))+(idx4-24);
                        a = array_5d[idx0][idx1][idx2][idx3][idx4];

                        IF x%3 = 0 THEN
                            IF a is not null THEN
                                raise EXCEPTION '5-d array failed';
                            END IF;
                        ELSE
                            IF a is null THEN
                                raise EXCEPTION '5-d array failed';
                            END IF;
                            IF a != x THEN
                                raise EXCEPTION '5-d array failed';
                            END IF;
                        END IF;
                    END LOOP;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 26 LOOP
        FOR idx1 IN 21 .. 26 LOOP
            FOR idx2 IN 22 .. 26 LOOP
                FOR idx3 IN 23 .. 26 LOOP
                    FOR idx4 IN 24 .. 26 LOOP
                        FOR idx5 IN 25 .. 26 LOOP
                            x = array_6d[idx0][idx1][idx2][idx3][idx4][idx5];
                            a = 2*(3*(4*(5*(6*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23))+(idx4-24))+(idx5-25);

                            IF x%3 = 0 THEN
                                IF a is not null THEN
                                    raise EXCEPTION '6-d array failed';
                                END IF;
                            ELSE
                                IF a is null THEN
                                    raise EXCEPTION '6-d array failed';
                                END IF;
                                IF a != x THEN
                                    raise EXCEPTION '6-d array failed';
                                END IF;
                            END IF;
                        END LOOP;
                    END LOOP;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    null_array = echo(null_array);
    array_empty = echo(array_empty);
    array_1d = echo(array_1d);
    array_2d = echo(array_2d);
    array_3d = echo(array_3d);
    array_4d = echo(array_4d);
    array_5d = echo(array_5d);
    array_6d = echo(array_6d);

    IF null_array is not NULL THEN
        raise EXCEPTION 'null_array copy failed';
    END IF;

    IF array_empty != '{}'::int[] THEN
        raise EXCEPTION 'empty array copy failed';
    END IF;

    FOR idx0 IN 20 .. 21 LOOP
        x = idx0 - 20;
        a = array_1d[idx0];

        IF x%3 = 0 THEN
            IF a is not null THEN
                raise EXCEPTION '1-d array copy failed';
            END IF;
        ELSE
            IF a is null THEN
                raise EXCEPTION '1-d array copy failed';
            END IF;
            IF a != x THEN
                raise EXCEPTION '1-d array copy failed';
            END IF;
        END IF;
    END LOOP;

    FOR idx0 IN 20 .. 22 LOOP
        FOR idx1 IN 21 .. 22 LOOP
            x = 2*(idx0-20)+(idx1-21);
            a = array_2d[idx0][idx1];

            IF x%3 = 0 THEN
                IF a is not null THEN
                    raise EXCEPTION '2-d array copy failed';
                END IF;
            ELSE
                IF a is null THEN
                    raise EXCEPTION '2-d array copy failed';
                END IF;
                IF a != x THEN
                    raise EXCEPTION '2-d array copy failed';
                END IF;
            END IF;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 23 LOOP
        FOR idx1 IN 21 .. 23 LOOP
            FOR idx2 IN 22 .. 23 LOOP
                x = 2*(3*(idx0-20)+(idx1-21))+(idx2-22);
                a = array_3d[idx0][idx1][idx2];

                IF x%3 = 0 THEN
                    IF a is not null THEN
                        raise EXCEPTION '3-d array copy failed';
                    END IF;
                ELSE
                    IF a is null THEN
                        raise EXCEPTION '3-d array copy failed';
                    END IF;
                    IF a != x THEN
                        raise EXCEPTION '3-d array copy failed';
                    END IF;
                END IF;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 24 LOOP
        FOR idx1 IN 21 .. 24 LOOP
            FOR idx2 IN 22 .. 24 LOOP
                FOR idx3 IN 23 .. 24 LOOP
                    x = 2*(3*(4*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23);
                    a = array_4d[idx0][idx1][idx2][idx3];

                    IF x%3 = 0 THEN
                        IF a is not null THEN
                            raise EXCEPTION '4-d array copy failed';
                        END IF;
                    ELSE
                        IF a is null THEN
                            raise EXCEPTION '4-d array copy failed';
                        END IF;
                        IF a != x THEN
                            raise EXCEPTION '4-d array copy failed';
                        END IF;
                    END IF;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 25 LOOP
        FOR idx1 IN 21 .. 25 LOOP
            FOR idx2 IN 22 .. 25 LOOP
                FOR idx3 IN 23 .. 25 LOOP
                    FOR idx4 IN 24 .. 25 LOOP
                        x = 2*(3*(4*(5*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23))+(idx4-24);
                        a = array_5d[idx0][idx1][idx2][idx3][idx4];

                        IF x%3 = 0 THEN
                            IF a is not null THEN
                                raise EXCEPTION '5-d array copy failed';
                            END IF;
                        ELSE
                            IF a is null THEN
                                raise EXCEPTION '5-d array copy failed';
                            END IF;
                            IF a != x THEN
                                raise EXCEPTION '5-d array copy failed';
                            END IF;
                        END IF;
                    END LOOP;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;

    FOR idx0 IN 20 .. 26 LOOP
        FOR idx1 IN 21 .. 26 LOOP
            FOR idx2 IN 22 .. 26 LOOP
                FOR idx3 IN 23 .. 26 LOOP
                    FOR idx4 IN 24 .. 26 LOOP
                        FOR idx5 IN 25 .. 26 LOOP
                            x = array_6d[idx0][idx1][idx2][idx3][idx4][idx5];
                            a = 2*(3*(4*(5*(6*(idx0-20)+(idx1-21))+(idx2-22))+(idx3-23))+(idx4-24))+(idx5-25);

                            IF x%3 = 0 THEN
                                IF a is not null THEN
                                    raise EXCEPTION '6-d array copy failed';
                                END IF;
                            ELSE
                                IF a is null THEN
                                    raise EXCEPTION '6-d array copy failed';
                                END IF;
                                IF a != x THEN
                                    raise EXCEPTION '6-d array copy failed';
                                END IF;
                            END IF;
                        END LOOP;
                    END LOOP;
                END LOOP;
            END LOOP;
        END LOOP;
    END LOOP;
END
$$;
