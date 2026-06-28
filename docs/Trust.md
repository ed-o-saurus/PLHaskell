# Trust

## Trusted

`plhaskell` is a _trusted_ language. The PostgreSQL manual explains:
<blockquote>
The optional key word TRUSTED specifies that the language does not grant access to data that the user would not otherwise have. Trusted languages are designed for ordinary database users (those without superuser privilege) and allows them to safely create functions and procedures.
</blockquote>

As such, unprivileged users are permitted to write and execute functions without the possibility that they will be able to access information or resources that are not allowed to access. This is accomplished by enforcing Haskell's strong type system.

## Untrusted

`plhaskellu` is an _untrusted_ language. It affords the user more flexibility but lacks the safeguards of the trusted variant. As such, only users with PostgreSQL superuser privilege can create functions with this variant.
