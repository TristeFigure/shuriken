{
Object x = $1;

if(x instanceof clojure.lang.ISeq)
    {
    clojure.lang.ISeq form = (clojure.lang.ISeq) x;
    Object op = clojure.lang.RT.first(form);
    if(isSpecial(op))
      return x;
    //macro expansion
    clojure.lang.Var v = isMacro(op);
    if(v != null)
      {
        // clojure.lang.Compiler.checkSpecs(v, form);

        try
          {
                    clojure.lang.ISeq args = clojure.lang.RT.cons(form, clojure.lang.RT.cons(clojure.lang.Compiler.LOCAL_ENV.get(), form.next()));
          return v.applyTo(args);
          }
        catch(clojure.lang.ArityException e)
          {
            // hide the 2 extra params for a macro
            throw new clojure.lang.ArityException(e.actual - 2, e.name);
          }
      } else
      {
      if(op instanceof clojure.lang.Symbol)
        {
        clojure.lang.Symbol sym = (clojure.lang.Symbol) op;
        String sname = sym.name;
        //(.substring s 2 5) => (. s substring 2 5)
        if(sym.name.charAt(0) == '.')
          {
          if(clojure.lang.RT.length(form) < 2)
            throw new IllegalArgumentException(
                "Malformed member expression, expecting (.member target ...)");
          clojure.lang.Symbol meth = clojure.lang.Symbol.intern(sname.substring(1));
          Object target = clojure.lang.RT.second(form);
          if(clojure.lang.Compiler.HostExpr.maybeClass(target, false) != null)
            {
            target = ((clojure.lang.IObj)clojure.lang.RT.list(IDENTITY, target)).withMeta(clojure.lang.RT.map((Object)clojure.lang.RT.TAG_KEY,(Object)clojure.lang.Compiler.CLASS));
            }
          return preserveTag(form, clojure.lang.RT.listStar(DOT, target, meth, form.next().next()));
          }
        else if(namesStaticMember(sym))
          {
          clojure.lang.Symbol target = clojure.lang.Symbol.intern(sym.ns);
          Class c = clojure.lang.Compiler.HostExpr.maybeClass(target, false);
          if(c != null)
            {
            clojure.lang.Symbol meth = clojure.lang.Symbol.intern(sym.name);
            return preserveTag(form, clojure.lang.RT.listStar(DOT, target, meth, form.next()));
            }
          }
        else
          {
          //(s.substring 2 5) => (. s substring 2 5)
          //also (package.class.name ...) (. package.class name ...)
          int idx = sname.lastIndexOf('.');
//          if(idx > 0 && idx < sname.length() - 1)
//            {
//            clojure.lang.Symbol target = clojure.lang.Symbol.intern(sname.substring(0, idx));
//            clojure.lang.Symbol meth = clojure.lang.Symbol.intern(sname.substring(idx + 1));
//            return clojure.lang.RT.listStar(DOT, target, meth, form.rest());
//            }
          //(StringBuilder. "foo") => (new StringBuilder "foo")
          //else
          if(idx == sname.length() - 1)
            return clojure.lang.RT.listStar(NEW, clojure.lang.Symbol.intern(sname.substring(0, idx)), form.next());
          }
        }
      }
    }
  return x;
}
