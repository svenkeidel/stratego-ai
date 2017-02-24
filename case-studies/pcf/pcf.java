import org.strategoxt.stratego_lib.*;
import org.strategoxt.lang.*;
import org.spoofax.interpreter.terms.*;
import static org.strategoxt.lang.Term.*;
import org.spoofax.interpreter.library.AbstractPrimitive;
import java.util.ArrayList;
import java.lang.ref.WeakReference;

@SuppressWarnings("all") public class pcf  
{ 
  protected static final boolean TRACES_ENABLED = true;

  protected static ITermFactory constantFactory;

  private static WeakReference<Context> initedContext;

  private static boolean isIniting;

  protected static IStrategoTerm constZero0;

  public static IStrategoConstructor _consConc_2;

  public static IStrategoConstructor _consNone_0;

  public static IStrategoConstructor _consSome_1;

  public static IStrategoConstructor _consCons_2;

  public static IStrategoConstructor _consNil_0;

  public static IStrategoConstructor _consIfz_3;

  public static IStrategoConstructor _consPred_1;

  public static IStrategoConstructor _consSucc_1;

  public static IStrategoConstructor _consZero_0;

  public static IStrategoConstructor _consAbs_2;

  public static IStrategoConstructor _consApp_2;

  public static IStrategoConstructor _consVar_1;

  public static Context init(Context context)
  { 
    synchronized(pcf.class)
    { 
      if(isIniting)
        return null;
      try
      { 
        isIniting = true;
        ITermFactory termFactory = context.getFactory();
        if(constantFactory == null)
        { 
          initConstructors(termFactory);
          initConstants(termFactory);
        }
        if(initedContext == null || initedContext.get() != context)
        { 
          org.strategoxt.stratego_lib.Main.init(context);
          context.registerComponent("pcf");
        }
        initedContext = new WeakReference<Context>(context);
        constantFactory = termFactory;
      }
      finally
      { 
        isIniting = false;
      }
      return context;
    }
  }

  public static Context init()
  { 
    return init(new Context());
  }

  public static void main(String args[])
  { 
    Context context = init();
    context.setStandAlone(true);
    try
    { 
      IStrategoTerm result;
      try
      { 
        result = context.invokeStrategyCLI(main_0_0.instance, "pcf", args);
      }
      finally
      { 
        context.getIOAgent().closeAllFiles();
      }
      if(result == null)
      { 
        System.err.println("pcf" + (TRACES_ENABLED ? ": rewriting failed, trace:" : ": rewriting failed"));
        context.printStackTrace();
        context.setStandAlone(false);
        System.exit(1);
      }
      else
      { 
        System.out.println(result);
        context.setStandAlone(false);
        System.exit(0);
      }
    }
    catch(StrategoErrorExit exit)
    { 
      context.setStandAlone(false);
      System.err.println(exit.getLocalizedMessage());
      System.exit(exit.getValue());
    }
    catch(StrategoExit exit)
    { 
      context.setStandAlone(false);
      System.exit(exit.getValue());
    }
  }

  public static IStrategoTerm mainNoExit(String ... args) throws StrategoExit
  { 
    return mainNoExit(new Context(), args);
  }

  public static IStrategoTerm mainNoExit(Context context, String ... args) throws StrategoExit
  { 
    try
    { 
      init(context);
      return context.invokeStrategyCLI(main_0_0.instance, "pcf", args);
    }
    finally
    { 
      context.getIOAgent().closeAllFiles();
    }
  }

  public static Strategy getMainStrategy()
  { 
    return main_0_0.instance;
  }

  public static void initConstructors(ITermFactory termFactory)
  { 
    _consConc_2 = termFactory.makeConstructor("Conc", 2);
    _consNone_0 = termFactory.makeConstructor("None", 0);
    _consSome_1 = termFactory.makeConstructor("Some", 1);
    _consCons_2 = termFactory.makeConstructor("Cons", 2);
    _consNil_0 = termFactory.makeConstructor("Nil", 0);
    _consIfz_3 = termFactory.makeConstructor("Ifz", 3);
    _consPred_1 = termFactory.makeConstructor("Pred", 1);
    _consSucc_1 = termFactory.makeConstructor("Succ", 1);
    _consZero_0 = termFactory.makeConstructor("Zero", 0);
    _consAbs_2 = termFactory.makeConstructor("Abs", 2);
    _consApp_2 = termFactory.makeConstructor("App", 2);
    _consVar_1 = termFactory.makeConstructor("Var", 1);
  }

  public static void initConstants(ITermFactory termFactory)
  { 
    constZero0 = termFactory.makeAppl(pcf._consZero_0, NO_TERMS);
  }

  @SuppressWarnings("all") public static class eval_0_0 extends Strategy 
  { 
    public static eval_0_0 instance = new eval_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      context.push("eval_0_0");
      Fail0:
      { 
        IStrategoTerm term0 = term;
        Success0:
        { 
          Fail1:
          { 
            if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
              break Fail1;
            IStrategoTerm arg1 = term.getSubterm(1);
            if(arg1.getTermType() != IStrategoTerm.APPL || pcf._consZero_0 != ((IStrategoAppl)arg1).getConstructor())
              break Fail1;
            term = pcf.constZero0;
            if(true)
              break Success0;
          }
          term = term0;
          IStrategoTerm term1 = term;
          Success1:
          { 
            Fail2:
            { 
              IStrategoTerm c_2 = null;
              IStrategoTerm d_2 = null;
              if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
                break Fail2;
              d_2 = term.getSubterm(0);
              IStrategoTerm arg2 = term.getSubterm(1);
              if(arg2.getTermType() != IStrategoTerm.APPL || pcf._consVar_1 != ((IStrategoAppl)arg2).getConstructor())
                break Fail2;
              c_2 = arg2.getSubterm(0);
              term = termFactory.makeTuple(c_2, d_2);
              term = lookup_0_0.instance.invoke(context, term);
              if(term == null)
                break Fail2;
              if(true)
                break Success1;
            }
            term = term1;
            IStrategoTerm term2 = term;
            Success2:
            { 
              Fail3:
              { 
                IStrategoTerm a_2 = null;
                IStrategoTerm b_2 = null;
                if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
                  break Fail3;
                IStrategoTerm arg3 = term.getSubterm(1);
                if(arg3.getTermType() != IStrategoTerm.APPL || pcf._consAbs_2 != ((IStrategoAppl)arg3).getConstructor())
                  break Fail3;
                a_2 = arg3.getSubterm(0);
                b_2 = arg3.getSubterm(1);
                term = termFactory.makeAppl(pcf._consAbs_2, new IStrategoTerm[]{a_2, b_2});
                if(true)
                  break Success2;
              }
              term = term2;
              IStrategoTerm term3 = term;
              Success3:
              { 
                Fail4:
                { 
                  IStrategoTerm s_1 = null;
                  IStrategoTerm t_1 = null;
                  IStrategoTerm u_1 = null;
                  IStrategoTerm v_1 = null;
                  IStrategoTerm w_1 = null;
                  IStrategoTerm x_1 = null;
                  IStrategoTerm y_1 = null;
                  if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
                    break Fail4;
                  v_1 = term.getSubterm(0);
                  IStrategoTerm arg4 = term.getSubterm(1);
                  if(arg4.getTermType() != IStrategoTerm.APPL || pcf._consApp_2 != ((IStrategoAppl)arg4).getConstructor())
                    break Fail4;
                  s_1 = arg4.getSubterm(0);
                  w_1 = arg4.getSubterm(1);
                  y_1 = term;
                  term = termFactory.makeTuple(v_1, s_1);
                  term = this.invoke(context, term);
                  if(term == null)
                    break Fail4;
                  if(term.getTermType() != IStrategoTerm.APPL || pcf._consAbs_2 != ((IStrategoAppl)term).getConstructor())
                    break Fail4;
                  t_1 = term.getSubterm(0);
                  u_1 = term.getSubterm(1);
                  term = termFactory.makeTuple(v_1, w_1);
                  term = this.invoke(context, term);
                  if(term == null)
                    break Fail4;
                  x_1 = term;
                  term = y_1;
                  IStrategoList list0;
                  list0 = checkListTail(v_1);
                  if(list0 == null)
                    break Fail4;
                  term = termFactory.makeTuple((IStrategoTerm)termFactory.makeListCons(termFactory.makeTuple(t_1, x_1), list0), u_1);
                  term = this.invoke(context, term);
                  if(term == null)
                    break Fail4;
                  if(true)
                    break Success3;
                }
                term = term3;
                IStrategoTerm term4 = term;
                Success4:
                { 
                  Fail5:
                  { 
                    IStrategoTerm o_1 = null;
                    IStrategoTerm p_1 = null;
                    if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
                      break Fail5;
                    o_1 = term.getSubterm(0);
                    IStrategoTerm arg5 = term.getSubterm(1);
                    if(arg5.getTermType() != IStrategoTerm.APPL || pcf._consSucc_1 != ((IStrategoAppl)arg5).getConstructor())
                      break Fail5;
                    p_1 = arg5.getSubterm(0);
                    term = termFactory.makeTuple(o_1, p_1);
                    term = this.invoke(context, term);
                    if(term == null)
                      break Fail5;
                    term = termFactory.makeAppl(pcf._consSucc_1, new IStrategoTerm[]{term});
                    if(true)
                      break Success4;
                  }
                  term = term4;
                  IStrategoTerm term5 = term;
                  Success5:
                  { 
                    Fail6:
                    { 
                      IStrategoTerm k_1 = null;
                      IStrategoTerm l_1 = null;
                      if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
                        break Fail6;
                      k_1 = term.getSubterm(0);
                      IStrategoTerm arg6 = term.getSubterm(1);
                      if(arg6.getTermType() != IStrategoTerm.APPL || pcf._consPred_1 != ((IStrategoAppl)arg6).getConstructor())
                        break Fail6;
                      l_1 = arg6.getSubterm(0);
                      term = termFactory.makeTuple(k_1, l_1);
                      term = this.invoke(context, term);
                      if(term == null)
                        break Fail6;
                      term = termFactory.makeAppl(pcf._consPred_1, new IStrategoTerm[]{term});
                      if(true)
                        break Success5;
                    }
                    term = term5;
                    IStrategoTerm term6 = term;
                    Success6:
                    { 
                      Fail7:
                      { 
                        IStrategoTerm g_1 = null;
                        IStrategoTerm h_1 = null;
                        IStrategoTerm i_1 = null;
                        if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
                          break Fail7;
                        h_1 = term.getSubterm(0);
                        IStrategoTerm arg7 = term.getSubterm(1);
                        if(arg7.getTermType() != IStrategoTerm.APPL || pcf._consIfz_3 != ((IStrategoAppl)arg7).getConstructor())
                          break Fail7;
                        i_1 = arg7.getSubterm(0);
                        g_1 = arg7.getSubterm(1);
                        term = termFactory.makeTuple(h_1, i_1);
                        term = this.invoke(context, term);
                        if(term == null)
                          break Fail7;
                        if(term.getTermType() != IStrategoTerm.APPL || pcf._consZero_0 != ((IStrategoAppl)term).getConstructor())
                          break Fail7;
                        term = termFactory.makeTuple(h_1, g_1);
                        term = this.invoke(context, term);
                        if(term == null)
                          break Fail7;
                        if(true)
                          break Success6;
                      }
                      term = term6;
                      IStrategoTerm b_1 = null;
                      IStrategoTerm c_1 = null;
                      IStrategoTerm d_1 = null;
                      if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
                        break Fail0;
                      c_1 = term.getSubterm(0);
                      IStrategoTerm arg8 = term.getSubterm(1);
                      if(arg8.getTermType() != IStrategoTerm.APPL || pcf._consIfz_3 != ((IStrategoAppl)arg8).getConstructor())
                        break Fail0;
                      d_1 = arg8.getSubterm(1);
                      b_1 = arg8.getSubterm(2);
                      term = termFactory.makeTuple(c_1, d_1);
                      term = this.invoke(context, term);
                      if(term == null)
                        break Fail0;
                      if(term.getTermType() != IStrategoTerm.APPL || pcf._consSucc_1 != ((IStrategoAppl)term).getConstructor())
                        break Fail0;
                      term = termFactory.makeTuple(c_1, b_1);
                      term = this.invoke(context, term);
                      if(term == null)
                        break Fail0;
                    }
                  }
                }
              }
            }
          }
        }
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class lookup_0_0 extends Strategy 
  { 
    public static lookup_0_0 instance = new lookup_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      ITermFactory termFactory = context.getFactory();
      context.push("lookup_0_0");
      Fail8:
      { 
        IStrategoTerm e_2 = null;
        IStrategoTerm f_2 = null;
        IStrategoTerm g_2 = null;
        IStrategoTerm term7 = term;
        Success7:
        { 
          Fail9:
          { 
            if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
              break Fail9;
            f_2 = term.getSubterm(0);
            IStrategoTerm arg10 = term.getSubterm(1);
            if(arg10.getTermType() != IStrategoTerm.LIST || ((IStrategoList)arg10).isEmpty())
              break Fail9;
            IStrategoTerm arg11 = ((IStrategoList)arg10).head();
            if(arg11.getTermType() != IStrategoTerm.TUPLE || arg11.getSubtermCount() != 2)
              break Fail9;
            if(arg11.getSubterm(0) != f_2 && !f_2.match(arg11.getSubterm(0)))
              break Fail9;
            e_2 = arg11.getSubterm(1);
            term = e_2;
            if(true)
              break Success7;
          }
          term = term7;
          if(term.getTermType() != IStrategoTerm.TUPLE || term.getSubtermCount() != 2)
            break Fail8;
          f_2 = term.getSubterm(0);
          IStrategoTerm arg13 = term.getSubterm(1);
          if(arg13.getTermType() != IStrategoTerm.LIST || ((IStrategoList)arg13).isEmpty())
            break Fail8;
          g_2 = ((IStrategoList)arg13).tail();
          term = termFactory.makeTuple(f_2, g_2);
          term = this.invoke(context, term);
          if(term == null)
            break Fail8;
        }
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  @SuppressWarnings("all") public static class main_0_0 extends Strategy 
  { 
    public static main_0_0 instance = new main_0_0();

    @Override public IStrategoTerm invoke(Context context, IStrategoTerm term)
    { 
      context.push("main_0_0");
      Fail10:
      { 
        term = eval_0_0.instance.invoke(context, term);
        if(term == null)
          break Fail10;
        context.popOnSuccess();
        if(true)
          return term;
      }
      context.popOnFailure();
      return null;
    }
  }

  public static void registerInterop(org.spoofax.interpreter.core.IContext context, Context compiledContext)
  { 
    new InteropRegisterer().registerLazy(context, compiledContext, InteropRegisterer.class.getClassLoader());
  }

  @SuppressWarnings("unused") public static class InteropRegisterer extends org.strategoxt.lang.InteropRegisterer 
  { 
    @Override public void register(org.spoofax.interpreter.core.IContext context, Context compiledContext)
    { 
      register(context, compiledContext, context.getVarScope());
    }

    @Override public void registerLazy(org.spoofax.interpreter.core.IContext context, Context compiledContext, ClassLoader classLoader)
    { 
      registerLazy(context, compiledContext, classLoader, context.getVarScope());
    }

    private void register(org.spoofax.interpreter.core.IContext context, Context compiledContext, org.spoofax.interpreter.core.VarScope varScope)
    { 
      compiledContext.registerComponent("pcf");
      pcf.init(compiledContext);
      varScope.addSVar("eval_0_0", new InteropSDefT(eval_0_0.instance, context));
      varScope.addSVar("lookup_0_0", new InteropSDefT(lookup_0_0.instance, context));
      varScope.addSVar("main_0_0", new InteropSDefT(main_0_0.instance, context));
    }

    private void registerLazy(org.spoofax.interpreter.core.IContext context, Context compiledContext, ClassLoader classLoader, org.spoofax.interpreter.core.VarScope varScope)
    { 
      compiledContext.registerComponent("pcf");
      pcf.init(compiledContext);
      varScope.addSVar("eval_0_0", new InteropSDefT(classLoader, "pcf$eval_0_0", context));
      varScope.addSVar("lookup_0_0", new InteropSDefT(classLoader, "pcf$lookup_0_0", context));
      varScope.addSVar("main_0_0", new InteropSDefT(classLoader, "pcf$main_0_0", context));
    }
  }
}