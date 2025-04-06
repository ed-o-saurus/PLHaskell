DO
$$
BEGIN
    IF alpha_func(1) <> ('abc', 42, NULL)::alpha THEN
        raise EXCEPTION 'alpha_func(1) failed';
    END IF;

    IF alpha_func(2) <> ('cde', NULL, 12.3)::alpha THEN
        raise EXCEPTION 'alpha_func(2) failed';
    END IF;

    IF alpha_func(3) <> (NULL, 42, 32.1)::alpha THEN
        raise EXCEPTION 'alpha_func(3) failed';
    END IF;

    IF alpha_func(4) is not NULL THEN
        raise EXCEPTION 'alpha_func(4) failed';
    END IF;
END
$$;
