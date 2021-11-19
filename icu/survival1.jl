using QuadGK
using GLMakie
using Polynomials
using ForwardDiff
using IntervalArithmetic
using DelimitedFiles

# Cubic hermite spline

struct CSpline{S,T}
    s::S; t::S; x::T; y::T; mx::T; my::T
end

"""
    CSpline(s, t, x, y = x, m0 = (y-x)/(t-s), m1 = (y-x)/(t-s))

Cubic spline parametrized by ``f(s) = x`` and ``f(t) = y``, ``f'(s) = m_0``, ``f'(t) = m_1``.
"""
CSpline(s, t, x::T, y = x, ms = (y-x)/(t-s), mt =  (y-x)/(t-s)) where {T} = CSpline{T,T}(s, t, x, y, ms, mt)
(cs::CSpline)(t) =  cspline(t, cs.s, cs.t, cs.x, cs.y, cs.mx, cs.my)
  

"""
    cspline(s, t1, t2, p1, p2, m1, m2)

Third degree polynomial evaluated in s in [t1, t2] with f(t1) = p1, f(t2) = p2, f'(t1) = m1, f'(t2) = m2
"""
function cspline(s, t1, t2, p1, p2, m1, m2)
    d = t2 - t1
    t = (s - t1)/(t2 - t1)
    t2 = t*t
    t3 = t2*t
    z1, z2, z3, z4 = (1 - 3*t2 + 2*t3,  3*t2 - 2*t3,  t - 2*t2 + t3, 0 - t2 + t3)
    z1*p1 + z2*p2 + z3*d*m1 + z4*d*m2
end


"""
    bounds(f, (start, end))
    bounds(f, start:step:end)
    
Compute density bounds using interval arithmetic on intervals given by the second argument.
"""
function bounds(f, r)
    extrema( 
         f(Interval(r[i],r[i+1])) 
        for i in eachindex(r)[1:end-1])
end


"""
    sampleS(f, T0, T, l=100)

Rejection sampler for a general density using the `bounds` function
"""    
function sampleS(f, T0, T, l=100)
    M = bounds(f, range(T0, T, length=l))[2].hi
    while true
        r = rand()
        t = rand()*T + T0
        if f(t) > M
            error("invalid bound")
        end
        if r*M < f(t)
            return t
        end
    end
end

"""
    condS(t, c)

Conditional expected remaining life years given survival until t for hazard c
"""
function condS(t, c)
    if c == 1.0
        Float64(Polynomials.integrate((variable(:x)-t)*f, t, 100))/S(t)
    else
        quadgk( x -> (x-t)*c*f(x)*(S(x))^(c-1), t, 100)[1]/(S(t)^(c))
    end
end


# First example 

T = 100 # maximal age

# survival function
# define a survival function as spline, arguments are 0, T, S(0) = 1, S(T) = 0, S'(0) <= 0 and S'(T) <= 0
# make it rational
# choice 1: S'(T) = -3/100
S0 = CSpline{Rational{Int},Rational{Int}}(0//1, T//1, 1//1, 0//1, 0//1, -3//100)
# choice 2: S'(T) = 0
#S0 = CSpline{Rational{Int},Rational{Int}}(0//1, T//1, 1//1, 0//1, 0//1, -0//1)

# get polynomial from S0
S = S0(variable(:x))
# density
f = -derivative(S)

# plotting ranges
s = range(0, T, length=1000)
s0 = s[2:end-1] # without endpoints

# proportional hazard
cwomen = 1.0 # equal to baseline hazard
cmen = exp(0.2) # increased hazard
cgeneral = exp(0.1)

# predicted life expectancies
# note: women life longer
@show condS(0, cwomen)
@show condS(0, cgeneral)
@show condS(0, cmen)



# compute sub-population survival function, pdf and hazard
Smen(s) = S(s)^cmen
Swomen(s) = S(s)
fwomen(s) = f(s)
fmen(s) =  cmen*(S.(s)).^(cmen - 1).*f.(s)
hwomen(s) = fwomen(s)/Swomen(s)
hmen(s) =  fmen(s)/Smen(s)

# figure 1
fig1 = Figure()
ax = Axis(fig1[1, 1], title="survival")
p1 = lines!(ax, s, Swomen.(s))
p2 = lines!(ax, s, Smen.(s))
ax = Axis(fig1[1, 2], title="density")
p3 = lines!(ax, s0, fwomen.(s0))
p4 = lines!(ax, s0, fmen.(s0))
ax = Axis(fig1[1, 3], title="hazard")
p5 = lines!(ax, s0, hwomen.(s0))
p6 = lines!(ax, s0, hmen.(s0))
Legend(fig1[1, 4], [p1, p2], ["a=1", "a=0"])
# https://stats.stackexchange.com/questions/481237/scaling-of-the-hazard-function-and-relationship-with-variance
save("fig1.png", fig1)

# sampling bounds test
bounds(fmen, range(0, T, length=100))
bounds(fwomen, range(0, T, length=100))


# sample the time of lifethreatening incident which we here assume has same distribution as death,
# should be actually happening a bit higher rate
n = 1000
men_age = [sampleS(fmen, 0,100) for i in 1:1000]
women_age = [sampleS(fwomen, 0,100) for i in 1:1000]
# Compute expected conditional remaining lifetime
men_remaining = condS.(men_age, cmen) 
women_remaining = condS.(women_age, cwomen)


# men are younger at incidence time 
@show mean(men_age), mean(women_age)

# surprise: woman may have less expected remaining life time after accident
@show mean(men_remaining), mean(women_remaining)
# so allocating ressources according to remaining lifetime allocates more resources to men

# compute expected conditional remaining lifetime from the model for the population
men_remaining2 = condS.(men_age, cgeneral) 
women_remaining2 = condS.(women_age, cgeneral)

# danger: now the expected survival time after incident only depends on age and is further underestimated
# the problem is 
@show mean(men_remaining2), mean(women_remaining2)

open("data1.txt"; write=true) do f
    write(f, "men_age women_age men_remaining women_remaining men_remaining2 women_remaining2\n")
    writedlm(f, [men_age women_age men_remaining women_remaining men_remaining2 women_remaining2 ], " ")
end