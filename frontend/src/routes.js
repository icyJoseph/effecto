import React, { Suspense, lazy } from "react";
import { BrowserRouter, Route, Switch } from "react-router-dom";
import Spinner from "./components/Spinner";
import NoMatch from "./containers/NoMatch";
import GlobalStyle from "./styles/GlobalStyle";

const LazyFwdAuth = lazy(() =>
  import(/*webpackChunkName: "lazyFwdAuth"*/ "./containers/FwdAuth")
);

const LazyMain = lazy(() =>
  import(/*webpackChunkName: "lazyMain"*/ "./containers/Main")
);

const LazyMenu = lazy(() =>
  import(/*webpackChunkName: "lazyMenu"*/ "./containers/Menu")
);

const LazyCreate = lazy(() =>
  import(/*webpackChunkName: "lazyMenu"*/ "./containers/Create")
);

const LazyJoin = lazy(() =>
  import(/*webpackChunkName: "lazyMenu"*/ "./containers/Join")
);

function SuspenseFwdAuth({ ...props }) {
  return (
    <Suspense fallback={<Spinner />}>
      <LazyFwdAuth {...props} />
    </Suspense>
  );
}

function SuspenseMain({ ...props }) {
  return (
    <Suspense fallback={<Spinner />}>
      <LazyMain {...props} />
    </Suspense>
  );
}

function SuspenseMenu({ ...props }) {
  return (
    <Suspense fallback={<Spinner />}>
      <LazyMenu {...props} pageWrapId="main" outerContainerId="root" />
    </Suspense>
  );
}

function SuspenseCreate({ ...props }) {
  return (
    <Suspense fallback={<Spinner />}>
      <LazyCreate {...props} />
    </Suspense>
  );
}

function SuspenseJoin({ ...props }) {
  return (
    <Suspense fallback={<Spinner />}>
      <LazyJoin {...props} />
    </Suspense>
  );
}

const Routes = () => (
  <>
    <GlobalStyle />
    <BrowserRouter>
      <Route path="/:route?" component={SuspenseMenu} />
      <Switch>
        <Route path="/auth/callback/" component={SuspenseFwdAuth} />
        <Route path="/create" component={SuspenseCreate} />
        <Route path="/join" component={SuspenseJoin} />
        <Route path="/" component={SuspenseMain} />
        <Route component={NoMatch} />
      </Switch>
    </BrowserRouter>
  </>
);

export default Routes;
